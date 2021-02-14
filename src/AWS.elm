module AWS exposing (signRequest, awsTargetHeader, AWSTarget(..))

{-|

@docs signRequest, awsTargetHeader, AWSTarget

-}

import AWS.Internal
import AWS.Types exposing (..)
import Http
import Time
import Url
import Url.Builder


{-| Options for AWS.awsTargetHeader
-}
type AWSTarget
    = DynamoDB_GetItem
    | DynamoDB_PutItem
    | DynamoDB_DeleteItem
    | DynamoDB_Query


{-| Adds necessary `X-Amz-Target` http request header

See <https://docs.aws.amazon.com/apigateway/api-reference/making-http-requests/>

-}
awsTargetHeader : AWSTarget -> ( String, String )
awsTargetHeader target =
    let
        value =
            "DynamoDB_20120810."
                ++ (case target of
                        DynamoDB_GetItem ->
                            "GetItem"

                        DynamoDB_PutItem ->
                            "PutItem"

                        DynamoDB_DeleteItem ->
                            "DeleteItem"

                        DynamoDB_Query ->
                            "Query"
                   )
    in
    ( "X-Amz-Target", value )


{-| Given an unsigned request, this function returns a SignedRequest record to be passed to `Http.task`
<https://package.elm-lang.org/packages/elm/http/latest/Http#task>

    AWS.signRequest config now rawRequest
        |> Result.map Http.task

    -- Result String (Task x a)

-}
signRequest :
    Config
    -> Time.Posix
    -> UnsignedRequest x a
    -> Result String (SignedRequest x a)
signRequest config now { method, headers, query, stringBody, resolver, service } =
    let
        contentType =
            headers
                -- we try to use any content-type header given
                |> List.filter (\( k, v ) -> String.toLower k == "content-type")
                |> List.head
                |> Maybe.map Tuple.second
                -- otherwise default to json
                |> Maybe.withDefault "application/json; charset=utf-8"

        signatureResult =
            AWS.Internal.sign
                config
                service
                now
                { headers = headers
                , method = method
                , payload = stringBody
                , query = query
                }
    in
    case signatureResult of
        Err err ->
            Err err

        Ok signature ->
            let
                finalHeaders =
                    ( "Authorization", AWS.Internal.authorizationHeader config signature )
                        :: signature.headers

                httpHeaders =
                    finalHeaders
                        -- Http.Body includes content-type header; avoid duplicating it here
                        |> List.filter (\( k, v ) -> String.toLower k /= "content-type")
                        -- xhr2 doesn't allow overwriting `Host`
                        |> List.filter (\( k, v ) -> String.toLower k /= "host")
                        |> List.map (\( k, v ) -> Http.header k v)

                queryParams =
                    case
                        String.toList <|
                            Url.Builder.toQuery <|
                                List.map (\( k, v ) -> Url.Builder.string k v) query
                    of
                        '?' :: xs ->
                            Just (String.fromList xs)

                        _ ->
                            Nothing

                endpointUrl =
                    AWS.Internal.endpoint config service
                        |> (\url -> { url | query = queryParams })
                        |> Url.toString

                signedRequest =
                    { method = method
                    , headers = httpHeaders
                    , url = endpointUrl
                    , body = Http.stringBody contentType stringBody
                    , resolver = resolver
                    , timeout = config.timeout
                    }
            in
            Ok signedRequest
