module AWS.SES exposing
    ( OutgoingMail(..), Response(..), sendOutgoingMail
    , paramsForMail, decodeResponse
    )

{-| Implementation of <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-requests.html>

@docs OutgoingMail, Response, sendOutgoingMail


## Tested internals

@docs paramsForMail, decodeResponse

-}

import AWS
import AWS.DynamoDB
import AWS.Internal
import AWS.Types exposing (..)
import Base64
import Dict
import Http
import Json.Decode
import Json.Encode
import Platform exposing (Task)
import String exposing (toInt)
import Task
import Time
import Xml.Decode


{-| -}
type OutgoingMail
    = RawEmail
        { from : String
        , destinations : List String
        , rawMessage : String
        }
    | Email
        { from : String
        , to : List String
        , replyTo : List String
        , subject : String
        , textBody : String
        , htmlBody : String
        }


indexedTuples prefix list =
    -- e.g. [("Destination.ToAddresses.member.1", "foobar@example.com")]
    List.indexedMap (\i s -> ( prefix ++ String.fromInt (i + 1), s )) list


givenEmail =
    Email
        { from = "alice@example.com"
        , to = [ "bob@example.com" ]
        , replyTo = [ "donotreply@example.com" ]
        , subject = "Test"
        , textBody = "Message sent using SendEmail"
        , htmlBody = "<p>Message sent using SendEmail</p>"
        }


{-|

    givenRawMessage : String
    givenRawMessage =
        -- NOTE: the leading space of this string will be removed
        "\n\nFrom:user@example.com\nSubject: Test\n\nMessage sent using SendRawEmail.\n\n"

    expectedRawParams : List (String, String)
    expectedRawParams =
        [ ("Action", "SendRawEmail")
        , ( "Source", "charlie@example.com" )
        , ("Destinations.member.1", "alice@example.com")
        , ("Destinations.member.2", "bob@example.com")
        , ("RawMessage.Data", "RnJvbTp1c2VyQGV4YW1wbGUuY29tClN1YmplY3Q6IFRlc3QKCk1lc3NhZ2Ugc2VudCB1c2luZyBTZW5kUmF3RW1haWwuCgo=")
        ]


    paramsForMail
        (RawEmail
            { from = "charlie@example.com"
            , destinations = ["alice@example.com", "bob@example.com"]
            , rawMessage = givenRawMessage
            }
        )
    --> Ok expectedRawParams

    givenEmailDetail : { from : String, to : List String, replyTo : List String, subject : String, textBody : String, htmlBody : String}
    givenEmailDetail =
        { from = "alice@example.com"
        , to = [ "bob@example.com" ]
        , replyTo = [ "donotreply@example.com" ]
        , subject = "Test"
        , textBody = "Message sent using SendEmail"
        , htmlBody = "<p>Message sent using SendEmail</p>"
        }

    expectedEmailParams : List (String, String)
    expectedEmailParams =
        [ ( "Action", "SendEmail" )
        , ( "Source", "alice@example.com" )
        , ( "Message.Subject.Data", "Test" )
        , ( "Message.Body.Text.Data", "Message sent using SendEmail" )
        , ( "Message.Body.Html.Data", "<p>Message sent using SendEmail</p>" )
        , ( "Destination.ToAddresses.member.1", "bob@example.com" )
        , ( "Destination.ReplyToAddresses.member.1", "donotreply@example.com" )
        ]

    paramsForMail (Email givenEmailDetail)
    --> Ok expectedEmailParams

-}
paramsForMail : OutgoingMail -> Result String (List ( String, String ))
paramsForMail mail =
    case mail of
        RawEmail { from, destinations, rawMessage } ->
            case Base64.encode (List.map Char.toCode (String.toList (String.trimLeft rawMessage))) of
                Ok s ->
                    Ok
                        (List.concat
                            [ [ ( "Action", "SendRawEmail" )
                              , ( "Source", from )
                              ]
                            , indexedTuples "Destinations.member." destinations
                            , [ ( "RawMessage.Data", s ) ]
                            ]
                        )

                Err err ->
                    Err err

        Email { from, to, replyTo, subject, textBody, htmlBody } ->
            Ok
                (List.concat
                    [ [ ( "Action", "SendEmail" )
                      , ( "Source", from )
                      , ( "Message.Subject.Data", subject )
                      , ( "Message.Body.Text.Data", textBody )
                      , ( "Message.Body.Html.Data", htmlBody )
                      ]
                    , indexedTuples "Destination.ToAddresses.member." to
                    , indexedTuples "Destination.ReplyToAddresses.member." replyTo
                    ]
                )


{-| Task that sends email with SES, e.g.

    import Http
    import Task
    import Time
    import AWS.Types exposing (..)

    awsCfg : AWS.Types.Config
    awsCfg =
        { awsSecretAccessKey = "topsecret"
        , accessKeyId = "topsecret"
        , awsRegion = "us-east-1"
        , service = ServiceSES
        }

    now : Time.Posix
    now =
        Time.millisToPosix 0

    sendOutgoingMail awsCfg now
        { from = "alice@example.com"
        , to = "bob@example.com"
        , replyTo = "alice@example.com"
        , subject = "Hi, I'd like to join your LinkedIn network."
        , htmlBody = "<a href='/spam'>Report spam</a>"
        }

-}
sendOutgoingMail : AWS.Types.Config -> Time.Posix -> OutgoingMail -> Task Http.Error Response
sendOutgoingMail awsConfig now outgoingMail =
    let
        signResult =
            unsignedRequest outgoingMail
                |> Result.andThen (AWS.signRequest { awsConfig | service = AWS.Types.ServiceSES } now)
    in
    case signResult of
        Err err ->
            Task.fail (Http.BadUrl err)

        Ok task ->
            Http.task task


unsignedRequest : OutgoingMail -> Result String (UnsignedRequest Http.Error Response)
unsignedRequest outgoingMail =
    let
        toUnsignedRequest params =
            let
                stringBody =
                    params
                        |> List.map (\( k, v ) -> k ++ "=" ++ AWS.Internal.awsPercentEncode v)
                        |> String.join "&"
            in
            { method = "POST"
            , headers = [ ( "Content-Type", "application/x-www-form-urlencoded" ) ]
            , query = []
            , stringBody = stringBody
            , resolver = Http.stringResolver (AWS.Internal.decodeHttpResponse (Xml.Decode.decodeString decodeResponse) identity)
            , timeout = Just 30000
            }
    in
    paramsForMail outgoingMail
        |> Result.map toUnsignedRequest


{-| Response from SES API

See <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-responses.html>

-}
type Response
    = Error { type_ : String, code : String, message : String }
    | Success { messageId : String, requestId : String }


{-|

    import Xml.Decode

    --
    -- Success scenario
    """
    <SendEmailResponse xmlns="https://email.amazonaws.com/doc/2010-03-31/">
      <SendEmailResult>
        <MessageId>000001271b15238a-fd3ae762-2563-11df-8cd4-6d4e828a9ae8-000000</MessageId>
      </SendEmailResult>
      <ResponseMetadata>
        <RequestId>fd3ae762-2563-11df-8cd4-6d4e828a9ae8</RequestId>
      </ResponseMetadata>
    </SendEmailResponse>
    """
    |> Xml.Decode.run decodeResponse
    --> Ok (Success  { messageId = "000001271b15238a-fd3ae762-2563-11df-8cd4-6d4e828a9ae8-000000", requestId = "fd3ae762-2563-11df-8cd4-6d4e828a9ae8" })

    --
    -- Error scenario
    """
    <ErrorResponse>
       <Error>
          <Type>
             Sender
          </Type>
          <Code>
             ValidationError
          </Code>
          <Message>
             Value null at 'message.subject' failed to satisfy constraint: Member must not be null
          </Message>
       </Error>
       <RequestId>
          42d59b56-7407-4c4a-be0f-4c88daeea257
       </RequestId>
    </ErrorResponse>
    """
    |> Xml.Decode.run decodeResponse
    --> Ok (Error  { type_ = "Sender", code = "ValidationError", message = "Value null at 'message.subject' failed to satisfy constraint: Member must not be null" })

-}
decodeResponse : Xml.Decode.Decoder Response
decodeResponse =
    Xml.Decode.oneOf
        [ -- Error
          Xml.Decode.succeed (\type_ code message -> Error { type_ = type_, code = code, message = message })
            |> Xml.Decode.requiredPath [ "Error", "Type" ] (Xml.Decode.single (Xml.Decode.string |> Xml.Decode.map String.trim))
            |> Xml.Decode.requiredPath [ "Error", "Code" ] (Xml.Decode.single (Xml.Decode.string |> Xml.Decode.map String.trim))
            |> Xml.Decode.requiredPath [ "Error", "Message" ] (Xml.Decode.single (Xml.Decode.string |> Xml.Decode.map String.trim))

        -- Email
        , Xml.Decode.succeed (\messageId requestId -> Success { messageId = messageId, requestId = requestId })
            |> Xml.Decode.requiredPath [ "SendEmailResult", "MessageId" ] (Xml.Decode.single Xml.Decode.string)
            |> Xml.Decode.requiredPath [ "ResponseMetadata", "RequestId" ] (Xml.Decode.single Xml.Decode.string)

        -- RawEmail
        , Xml.Decode.succeed (\messageId requestId -> Success { messageId = messageId, requestId = requestId })
            |> Xml.Decode.requiredPath [ "SendRawEmailResult", "MessageId" ] (Xml.Decode.single Xml.Decode.string)
            |> Xml.Decode.requiredPath [ "ResponseMetadata", "RequestId" ] (Xml.Decode.single Xml.Decode.string)
        ]
