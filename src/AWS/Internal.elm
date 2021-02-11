module AWS.Internal exposing (..)

import AWS.Types exposing (..)
import Base16
import Crypto.HMAC
import Crypto.Hash
import DateFormat
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Time
import Url
import Url.Builder
import Word.Bytes


endpoint : Config -> Url.Url
endpoint { service, awsRegion } =
    let
        urlWithoutRegion =
            { protocol = Url.Https
            , host = serviceName service ++ ".amazonaws.com"
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }
    in
    case service of
        ServiceIam ->
            urlWithoutRegion

        ServiceDynamoDB ->
            { urlWithoutRegion | host = serviceName service ++ "." ++ awsRegion ++ ".amazonaws.com" }

        ServiceSES ->
            { urlWithoutRegion | host = serviceName service ++ "." ++ awsRegion ++ ".amazonaws.com" }


serviceName : Service -> String
serviceName service =
    case service of
        ServiceIam ->
            "iam"

        ServiceDynamoDB ->
            "dynamodb"

        ServiceSES ->
            "email"


algorithm =
    "AWS4-HMAC-SHA256"


authorizationHeader : Config -> Signature -> String
authorizationHeader config signature =
    String.join ""
        [ algorithm
        , " Credential="
        , config.accessKeyId
        , "/"
        , signature.credentialScope
        , ", SignedHeaders="
        , signature.signedHeaders
        , ", Signature="
        , signature.text
        ]


{-| Signing AWS requests with Signature Version 4 <https://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html>

    import Time
    import AWS.Types exposing (..)

    nowMillisecond : Int
    nowMillisecond =
        1440938160000

    requestDateTime : Time.Posix
    requestDateTime =
        Time.millisToPosix nowMillisecond

    awsConfig : Config
    awsConfig =
        { awsSecretAccessKey = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
        , awsRegion = "us-east-1"
        , accessKeyId = ""
        , service = ServiceIam
        }

    sign
        awsConfig
        requestDateTime
        { method = "GET"
        , query =
            [ ( "Action", "ListUsers" )
            , ( "Version", "2010-05-08" )
            ]
        , headers =
            [ ( "Content-Type", "application/x-www-form-urlencoded; charset=utf-8" )
            ]
        , payload = ""
        } |> Result.map .text
    --> Ok "5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"

NOTE: this function is only exposed to allow testing

-}
sign :
    Config
    -> Time.Posix
    ->
        { method : String
        , query : List ( String, String )
        , headers : List ( String, String )
        , payload : String
        }
    -> Result String Signature
sign config now request =
    let
        url =
            endpoint config

        yyyymmddThhmmssz =
            DateFormat.format
                [ DateFormat.yearNumber
                , DateFormat.monthFixed
                , DateFormat.dayOfMonthFixed
                , DateFormat.text "T"
                , DateFormat.hourMilitaryFixed
                , DateFormat.minuteFixed
                , DateFormat.secondFixed
                , DateFormat.text "Z"
                ]
                Time.utc
                now

        yyyymmdd =
            DateFormat.format
                [ DateFormat.yearNumber
                , DateFormat.monthFixed
                , DateFormat.dayOfMonthFixed
                ]
                Time.utc
                now

        headers =
            request.headers
                |> List.append
                    [ ( "X-Amz-Date", yyyymmddThhmmssz )
                    , ( "Host", url.host )
                    ]

        credentialScope =
            String.join "/" [ yyyymmdd, config.awsRegion, serviceName config.service, "aws4_request" ]

        signedHeaders =
            canonicalHeaderKeys headers

        canonicalRequest =
            String.join "\n"
                [ request.method
                , url.path
                , canonicalQuery request.query
                , canonicalHeaders headers
                , signedHeaders
                , Crypto.Hash.sha256 request.payload
                ]

        stringToSign =
            String.join "\n"
                [ algorithm
                , yyyymmddThhmmssz
                , credentialScope
                , Crypto.Hash.sha256 canonicalRequest
                ]

        signingKey =
            Word.Bytes.fromUTF8 ("AWS4" ++ config.awsSecretAccessKey)
                |> (\k -> Crypto.HMAC.digestBytes Crypto.HMAC.sha256 k (Word.Bytes.fromUTF8 yyyymmdd))
                |> (\k -> Crypto.HMAC.digestBytes Crypto.HMAC.sha256 k (Word.Bytes.fromUTF8 config.awsRegion))
                |> (\k -> Crypto.HMAC.digestBytes Crypto.HMAC.sha256 k (Word.Bytes.fromUTF8 (serviceName config.service)))
                |> (\k -> Crypto.HMAC.digestBytes Crypto.HMAC.sha256 k (Word.Bytes.fromUTF8 "aws4_request"))

        result =
            case List.head (List.filter (\( k, v ) -> String.toLower k == "host") request.headers) of
                Nothing ->
                    -- ok, we can proceed to sign
                    Crypto.HMAC.digestBytes Crypto.HMAC.sha256 signingKey (Word.Bytes.fromUTF8 stringToSign)
                        |> Base16.encode

                Just s ->
                    -- since the sign function is already a Result, let's fail explicitly
                    Err "Manually specifying `Host` header is not allowed by xhr2"
    in
    case result of
        Err err ->
            Err err

        Ok text ->
            Ok
                { text = String.toLower text
                , credentialScope = credentialScope
                , headers = headers
                , signedHeaders = signedHeaders
                , algorithm = algorithm
                }


canonicalQuery : List ( String, String ) -> String
canonicalQuery keyValues =
    List.sortBy (\( k, v ) -> k ++ v) keyValues
        |> List.map (\( k, v ) -> awsPercentEncode k ++ "=" ++ awsPercentEncode v)
        |> String.join "&"


canonicalHeaders : List ( String, String ) -> String
canonicalHeaders keyValues =
    List.map (\( k, v ) -> ( String.toLower k, String.trim v )) keyValues
        |> List.sortBy Tuple.first
        |> List.map (\( k, v ) -> k ++ ":" ++ v ++ "\n")
        |> String.join ""


canonicalHeaderKeys : List ( String, String ) -> String
canonicalHeaderKeys keyValues =
    List.map (\( k, v ) -> String.toLower k) keyValues
        |> List.sort
        |> String.join ";"


{-|

    awsPercentEncode "Az09-_.~!*'()"
    --> "Az09-_.~%21%2A%27%28%29"

    awsPercentEncode "space "
    --> "space%20"

    awsPercentEncode "🙂"
    --> "%F0%9F%99%82"

    awsPercentEncode "equal=sign"
    --> "equal%3Dsign"

    awsPercentEncode "bob@example.com"
    --> "bob%40example.com"

-}
awsPercentEncode : String -> String
awsPercentEncode string =
    Url.percentEncode string
        |> String.replace "!" "%21"
        |> String.replace "*" "%2A"
        |> String.replace "'" "%27"
        |> String.replace "(" "%28"
        |> String.replace ")" "%29"


{-| If http response body does not contain expected String, return as Http.BadBody error
-}
stringMatchHttpResponse : String -> Http.Response String -> Result Http.Error ()
stringMatchHttpResponse needle resp =
    let
        matchResult haystack =
            if String.contains needle haystack then
                Ok ()

            else
                Err (Http.BadBody ("[" ++ needle ++ "] not found: " ++ haystack))
    in
    case resp of
        Http.GoodStatus_ m s ->
            matchResult s

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            Err (Http.BadStatus m.statusCode)


{-| applies a decoder to response of `Http.task`

To do JSON decoding:

    Http.task
        { resolver = Http.stringResolver (decodeHttpResponse (Json.Decode.decodeString thingDecoder) Json.Decode.errorToString)
        , ...
        }

To do XML decoding

    Http.task
        { resolver = Http.stringResolver (decodeHttpResponse (Xml.Decode.decodeString thingDecoder) identity)
        , ...
        }

-}
decodeHttpResponse : (b -> Result e a) -> (e -> String) -> Http.Response b -> Result Http.Error a
decodeHttpResponse decode errorToString resp =
    case resp of
        Http.GoodStatus_ m s ->
            decode s
                |> Result.mapError (errorToString >> Http.BadBody)

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ m s ->
            decode s
                -- best effort attempt to decode the response body, but fallback to `Http.BadStatus`
                |> Result.mapError (always (Http.BadStatus m.statusCode))
