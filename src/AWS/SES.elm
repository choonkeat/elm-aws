module AWS.SES exposing
    ( OutgoingMail(..), Response(..), unsignedRequest, Notification(..), decodeNotification
    , Mail, decodeMail, CommonMailHeaders, decodeCommonMailHeaders, Bounce, BounceRecipient, BounceType(..), ComplainedRecipient, Complaint, ComplaintFeedbackType(..), ComplaintSubType(..), Delivery, NamedValue, decodeBounce, decodeBounceRecipient, decodeBounceType, decodeComplainedRecipient, decodeComplaint, decodeComplaintFeedbackType, decodeComplaintSubType, decodeDelivery, decodeNamedValue
    , paramsForMail, decodeResponse
    )

{-| Implementation of <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-requests.html>

@docs OutgoingMail, Response, unsignedRequest, Notification, decodeNotification


# Inner types and decoders

@docs Mail, decodeMail, CommonMailHeaders, decodeCommonMailHeaders, Bounce, BounceRecipient, BounceType, ComplainedRecipient, Complaint, ComplaintFeedbackType, ComplaintSubType, Delivery, NamedValue, decodeBounce, decodeBounceRecipient, decodeBounceType, decodeComplainedRecipient, decodeComplaint, decodeComplaintFeedbackType, decodeComplaintSubType, decodeDelivery, decodeNamedValue


# Tested internals

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
        , ( "ReplyToAddresses.member.1", "donotreply@example.com" )
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
                    , indexedTuples "ReplyToAddresses.member." replyTo
                    ]
                )


{-| Construct an `UnsignedRequest` for SES, e.g.

    import Http
    import AWS.Types

    unsignedResult : Result String (AWS.Types.UnsignedRequest Http.Error Response)
    unsignedResult =
        unsignedRequest
            (Email
                { from = "alice@example.com"
                , to = [ "bob@example.com" ]
                , replyTo = [ "donotreply@example.com" ]
                , subject = "Test"
                , textBody = "Message sent using SendEmail"
                , htmlBody = "<p>Message sent using SendEmail</p>"
                }
            )

    Result.map .method unsignedResult
    --> Ok "POST"

    Result.map .headers unsignedResult
    --> Ok [("Content-Type","application/x-www-form-urlencoded")]

    Result.map .stringBody unsignedResult
    --> Ok "Action=SendEmail&Source=alice%40example.com&Message.Subject.Data=Test&Message.Body.Text.Data=Message%20sent%20using%20SendEmail&Message.Body.Html.Data=%3Cp%3EMessage%20sent%20using%20SendEmail%3C%2Fp%3E&Destination.ToAddresses.member.1=bob%40example.com&ReplyToAddresses.member.1=donotreply%40example.com"

    Result.map .service unsignedResult
    --> Ok AWS.Types.ServiceSES


    usage config now unsignedResult =
        unsignedResult
            |> Result.andThen (AWS.signRequest config now)
            |> Result.map Http.task

-}
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
            , service = AWS.Types.ServiceSES
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



--


andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
andMap =
    Json.Decode.map2 (|>)


{-| The top-level JSON object in an Amazon SES notification <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#top-level-json-object>
-}
type Notification
    = SESBounce Mail Bounce
    | SESComplaint Mail Complaint
    | SESDelivery Mail Delivery


{-| decoder of an Amazon SES notification according to <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html>
-}
decodeNotification : Json.Decode.Decoder Notification
decodeNotification =
    let
        decode : ( String, Mail ) -> Json.Decode.Decoder Notification
        decode ( notificationType, mail ) =
            case notificationType of
                "Bounce" ->
                    Json.Decode.map (SESBounce mail) (Json.Decode.field "bounce" decodeBounce)

                "Complaint" ->
                    Json.Decode.map (SESComplaint mail) (Json.Decode.field "complaint" decodeComplaint)

                "Delivery" ->
                    Json.Decode.map (SESDelivery mail) (Json.Decode.field "delivery" decodeDelivery)

                _ ->
                    Json.Decode.fail ("Invalidation notificationType: " ++ notificationType)
    in
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "notificationType" Json.Decode.string)
        (Json.Decode.field "mail" decodeMail)
        |> Json.Decode.andThen decode


{-| `mail` in top-level JSON object
<https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#mail-object>
-}
type alias Mail =
    { timestamp : Time.Posix
    , messageId : String
    , source : String
    , sourceArn : String
    , sourceIp : String
    , sendingAccountId : String
    , destination : List String
    , headersTruncated : Bool
    , headers : List NamedValue
    , commonHeaders : CommonMailHeaders
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#mail-object>
-}
decodeMail : Json.Decode.Decoder Mail
decodeMail =
    Json.Decode.succeed Mail
        |> andMap (Json.Decode.field "timestamp" AWS.Internal.decodeTimePosix)
        |> andMap (Json.Decode.field "messageId" Json.Decode.string)
        |> andMap (Json.Decode.field "source" Json.Decode.string)
        |> andMap (Json.Decode.field "sourceArn" Json.Decode.string)
        |> andMap (Json.Decode.field "sourceIp" Json.Decode.string)
        |> andMap (Json.Decode.field "sendingAccountId" Json.Decode.string)
        |> andMap (Json.Decode.field "destination" (Json.Decode.list Json.Decode.string))
        |> andMap (Json.Decode.field "headersTruncated" Json.Decode.bool)
        |> andMap (Json.Decode.field "headers" (Json.Decode.list decodeNamedValue))
        |> andMap (Json.Decode.field "commonHeaders" decodeCommonMailHeaders)


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#mail-object>
-}
type alias CommonMailHeaders =
    { from : List String
    , date : Maybe Time.Posix
    , to : List String
    , messageId : Maybe String
    , subject : Maybe String
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#mail-object>
-}
decodeCommonMailHeaders : Json.Decode.Decoder CommonMailHeaders
decodeCommonMailHeaders =
    Json.Decode.succeed CommonMailHeaders
        |> andMap (Json.Decode.field "from" (Json.Decode.list Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "date" AWS.Internal.decodeTimePosix))
        |> andMap (Json.Decode.field "to" (Json.Decode.list Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "messageId" Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "subject" Json.Decode.string))


{-| Mail headers like

      {
         "name":"From",
         "value":"\"Sender Name\" <sender@example.com>"
      }

-}
type alias NamedValue =
    { name : String
    , value : String
    }


{-| decoder for Mail headers
-}
decodeNamedValue : Json.Decode.Decoder NamedValue
decodeNamedValue =
    Json.Decode.map2 NamedValue
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "value" Json.Decode.string)


{-| `complaint` in top-level JSON object
<https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complaint-object>
-}
type ComplaintFeedbackType
    = ComplaintFeedbackAbuse
    | ComplaintFeedbackAuthFailure
    | ComplaintFeedbackFraud
    | ComplaintFeedbackNotSpam
    | ComplaintFeedbackVirus
    | ComplaintFeedbackOther
    | ComplaintFeedbackUnknown String


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complaint-object>
-}
type alias Complaint =
    { userAgent : Maybe String
    , complaintFeedbackType : Maybe ComplaintFeedbackType
    , arrivalDate : Maybe Time.Posix
    , complainedRecipients : List ComplainedRecipient
    , timestamp : Time.Posix
    , feedbackId : String
    , complaintSubType : Maybe ComplaintSubType
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complaint-object>
-}
decodeComplaint : Json.Decode.Decoder Complaint
decodeComplaint =
    Json.Decode.succeed Complaint
        |> andMap (Json.Decode.maybe (Json.Decode.field "userAgent" Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "complaintFeedbackType" decodeComplaintFeedbackType))
        |> andMap (Json.Decode.maybe (Json.Decode.field "arrivalDate" AWS.Internal.decodeTimePosix))
        |> andMap (Json.Decode.field "complainedRecipients" (Json.Decode.list decodeComplainedRecipient))
        |> andMap (Json.Decode.field "timestamp" AWS.Internal.decodeTimePosix)
        |> andMap (Json.Decode.field "feedbackId" Json.Decode.string)
        |> andMap (Json.Decode.maybe (Json.Decode.field "complaintSubType" decodeComplaintSubType))


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complaint-object>
-}
decodeComplaintFeedbackType : Json.Decode.Decoder ComplaintFeedbackType
decodeComplaintFeedbackType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "abuse" ->
                        Json.Decode.succeed ComplaintFeedbackAbuse

                    "auth-failure" ->
                        Json.Decode.succeed ComplaintFeedbackAuthFailure

                    "fraud" ->
                        Json.Decode.succeed ComplaintFeedbackFraud

                    "not-spam" ->
                        Json.Decode.succeed ComplaintFeedbackNotSpam

                    "virus" ->
                        Json.Decode.succeed ComplaintFeedbackVirus

                    "other" ->
                        Json.Decode.succeed ComplaintFeedbackOther

                    _ ->
                        Json.Decode.succeed (ComplaintFeedbackUnknown string)
            )


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complained-recipients>
-}
type alias ComplainedRecipient =
    { emailAddress : String
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complained-recipients>
-}
decodeComplainedRecipient : Json.Decode.Decoder ComplainedRecipient
decodeComplainedRecipient =
    Json.Decode.map ComplainedRecipient
        (Json.Decode.field "emailAddress" Json.Decode.string)


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complaint-object>
-}
type ComplaintSubType
    = OnAccountSuppressionList


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#complaint-object>
-}
decodeComplaintSubType : Json.Decode.Decoder ComplaintSubType
decodeComplaintSubType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "OnAccountSuppressionList" ->
                        Json.Decode.succeed OnAccountSuppressionList

                    _ ->
                        Json.Decode.fail ("Invalid ComplaintSubType: " ++ string)
            )


{-| `bounce` in top-level JSON object
<https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounce-object>
-}
type alias Bounce =
    { bounceType : BounceType
    , bouncedRecipients : List BounceRecipient
    , timestamp : Time.Posix
    , feedbackId : String
    , remoteMtaIp : Maybe String
    , reportingMTA : Maybe String
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounce-object>
-}
decodeBounce : Json.Decode.Decoder Bounce
decodeBounce =
    Json.Decode.succeed Bounce
        |> andMap decodeBounceType
        |> andMap (Json.Decode.field "bouncedRecipients" (Json.Decode.list decodeBounceRecipient))
        |> andMap (Json.Decode.field "timestamp" AWS.Internal.decodeTimePosix)
        |> andMap (Json.Decode.field "feedbackId" Json.Decode.string)
        |> andMap (Json.Decode.maybe (Json.Decode.field "remoteMtaIp" Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "reportingMTA" Json.Decode.string))


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounced-recipients>
-}
type alias BounceRecipient =
    { emailAddress : String
    , action : Maybe String
    , status : Maybe String
    , diagnosticCode : Maybe String
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounced-recipients>
-}
decodeBounceRecipient : Json.Decode.Decoder BounceRecipient
decodeBounceRecipient =
    Json.Decode.succeed BounceRecipient
        |> andMap (Json.Decode.field "emailAddress" Json.Decode.string)
        |> andMap (Json.Decode.maybe (Json.Decode.field "action" Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "status" Json.Decode.string))
        |> andMap (Json.Decode.maybe (Json.Decode.field "diagnosticCode" Json.Decode.string))


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounce-types>
-}
type BounceType
    = BounceUndetermined
    | BouncePermanent
    | BouncePermanentGeneral
    | BouncePermanentNoEmail
    | BouncePermanentSuppressed
    | BouncePermanentOnAccountSuppressionList
    | BounceTransient
    | BounceTransientGeneral
    | BounceTransientMailboxFull
    | BounceTransientMessageTooLarge
    | BounceTransientContentRejected
    | BounceTransientAttachmentRejected


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounce-types>
-}
decodeBounceType : Json.Decode.Decoder BounceType
decodeBounceType =
    let
        mapBounceTypeSubType : ( String, Maybe String ) -> Json.Decode.Decoder BounceType
        mapBounceTypeSubType ( bounceType, bounceSubType ) =
            case ( bounceType, bounceSubType ) of
                ( "Undetermined", _ ) ->
                    Json.Decode.succeed BounceUndetermined

                ( "Permanent", Nothing ) ->
                    Json.Decode.succeed BouncePermanent

                ( "Permanent", Just "General" ) ->
                    Json.Decode.succeed BouncePermanentGeneral

                ( "Permanent", Just "NoEmail" ) ->
                    Json.Decode.succeed BouncePermanentNoEmail

                ( "Permanent", Just "Suppressed" ) ->
                    Json.Decode.succeed BouncePermanentSuppressed

                ( "Permanent", Just "OnAccountSuppressionList" ) ->
                    Json.Decode.succeed BouncePermanentOnAccountSuppressionList

                ( "Transient", Nothing ) ->
                    Json.Decode.succeed BounceTransient

                ( "Transient", Just "General" ) ->
                    Json.Decode.succeed BounceTransientGeneral

                ( "Transient", Just "MailboxFull" ) ->
                    Json.Decode.succeed BounceTransientMailboxFull

                ( "Transient", Just "MessageTooLarge" ) ->
                    Json.Decode.succeed BounceTransientMessageTooLarge

                ( "Transient", Just "ContentRejected" ) ->
                    Json.Decode.succeed BounceTransientContentRejected

                ( "Transient", Just "AttachmentRejected" ) ->
                    Json.Decode.succeed BounceTransientAttachmentRejected

                ( _, _ ) ->
                    Json.Decode.fail ("Unknown bounceType " ++ bounceType ++ ", bounceSubType=" ++ Maybe.withDefault "<null>" bounceSubType)
    in
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "bounceType" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "bounceSubType" Json.Decode.string))
        |> Json.Decode.andThen mapBounceTypeSubType


{-| `delivery` in top-level JSON object
<https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#delivery-object>
-}
type alias Delivery =
    { timestamp : Time.Posix
    , processingTimeMillis : Int
    , recipients : List String
    }


{-| <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#delivery-object>
-}
decodeDelivery : Json.Decode.Decoder Delivery
decodeDelivery =
    Json.Decode.map3 Delivery
        (Json.Decode.field "timestamp" AWS.Internal.decodeTimePosix)
        (Json.Decode.field "processingTimeMillis" Json.Decode.int)
        (Json.Decode.field "recipients" (Json.Decode.list Json.Decode.string))
