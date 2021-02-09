module AWS.SES exposing (OutgoingMail, sendOutgoingMail)

{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-requests.html>
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


type alias OutgoingMail =
    { from : String
    , to : String
    , replyTo : String
    , subject : String
    , htmlBody : String
    }


sendOutgoingMail : AWS.Types.Config -> OutgoingMail -> Time.Posix -> Task Http.Error ()
sendOutgoingMail awsConfig outgoingMail now =
    case AWS.signRequest { awsConfig | service = AWS.Types.ServiceSES } now (unsignedRequest outgoingMail) of
        Err err ->
            Task.fail (Http.BadUrl err)

        Ok task ->
            Http.task task


{-| Response is XML <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-responses.html>
-}
unsignedRequest : OutgoingMail -> UnsignedRequest Http.Error ()
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
    , resolver = Http.stringResolver (AWS.Internal.stringMatchHttpResponse "MessageId")
    , timeout = Just 30000
    }
