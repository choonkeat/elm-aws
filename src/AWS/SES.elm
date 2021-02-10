module AWS.SES exposing (OutgoingMail, Response(..), decodeResponse, sendOutgoingMail)

{-| Implementation of <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-requests.html>

@docs OutgoingMail, Response, decodeResponse, sendOutgoingMail

-}

import AWS
import AWS.DynamoDB
import AWS.Internal
import AWS.Types exposing (..)
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
type alias OutgoingMail =
    { from : String
    , to : String
    , replyTo : String
    , subject : String
    , htmlBody : String
    }


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
    case AWS.signRequest { awsConfig | service = AWS.Types.ServiceSES } now (unsignedRequest outgoingMail) of
        Err err ->
            Task.fail (Http.BadUrl err)

        Ok task ->
            Http.task task


unsignedRequest : OutgoingMail -> UnsignedRequest Http.Error Response
unsignedRequest outgoingMail =
    let
        stringBody =
            [ ( "Action", "SendEmail" )
            , ( "Source", outgoingMail.from )
            , ( "Destination.ToAddresses.member.1", outgoingMail.to )
            , ( "Destination.ReplyToAddresses.member.1", outgoingMail.replyTo )
            , ( "Message.Subject.Data", outgoingMail.subject )
            , ( "Message.Body.Html.Data", outgoingMail.htmlBody )
            ]
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
    let
        decodeErrorResponse =
            Xml.Decode.succeed (\type_ code message -> Error { type_ = type_, code = code, message = message })
                |> Xml.Decode.requiredPath [ "Error", "Type" ] (Xml.Decode.single (Xml.Decode.string |> Xml.Decode.map String.trim))
                |> Xml.Decode.requiredPath [ "Error", "Code" ] (Xml.Decode.single (Xml.Decode.string |> Xml.Decode.map String.trim))
                |> Xml.Decode.requiredPath [ "Error", "Message" ] (Xml.Decode.single (Xml.Decode.string |> Xml.Decode.map String.trim))

        decodeSuccessResponse =
            Xml.Decode.succeed (\messageId requestId -> Success { messageId = messageId, requestId = requestId })
                |> Xml.Decode.requiredPath [ "SendEmailResult", "MessageId" ] (Xml.Decode.single Xml.Decode.string)
                |> Xml.Decode.requiredPath [ "ResponseMetadata", "RequestId" ] (Xml.Decode.single Xml.Decode.string)
    in
    Xml.Decode.oneOf
        [ decodeErrorResponse
        , decodeSuccessResponse
        ]
