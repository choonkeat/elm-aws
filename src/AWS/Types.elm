module AWS.Types exposing (Config, Service(..), SignedRequest, UnsignedRequest, Signature)

{-|

@docs Config, Service, SignedRequest, UnsignedRequest, Signature

-}

import Http


{-| -}
type alias Config =
    { awsSecretAccessKey : String
    , awsRegion : String
    , accessKeyId : String
    , timeout : Maybe Float
    }


{-| -}
type Service
    = ServiceIam
    | ServiceDynamoDB
    | ServiceSES
    | ServiceSQS


{-| -}
type alias UnsignedRequest x a =
    { method : String
    , headers : List ( String, String )
    , query : List ( String, String )
    , stringBody : String
    , resolver : Http.Resolver x a
    , service : Service
    }


{-| -}
type alias SignedRequest x a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , resolver : Http.Resolver x a
    , timeout : Maybe Float
    }


{-| -}
type alias Signature =
    { text : String
    , credentialScope : String
    , headers : List ( String, String )
    , signedHeaders : String
    , algorithm : String
    }
