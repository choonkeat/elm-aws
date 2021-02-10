module AWS.DynamoDB exposing
    ( Base64String
    , JsonEncoded(..)
    , QueryResponse
    , decodeB
    , decodeBOOL
    , decodeBS
    , decodeJsonEncoded
    , decodeL
    , decodeM
    , decodeN
    , decodeNS
    , decodeNULL
    , decodeQueryResponse
    , decodeS
    , decodeSS
    , encodeB
    , encodeBOOL
    , encodeBS
    , encodeJsonEncoded
    , encodeL
    , encodeM
    , encodeN
    , encodeNS
    , encodeNULL
    , encodeS
    , encodeSS
    , fromBase64
    , toBase64
    )

{-|

@docs Base64String
@docs JsonEncoded
@docs QueryResponse
@docs decodeB
@docs decodeBOOL
@docs decodeBS
@docs decodeJsonEncoded
@docs decodeL
@docs decodeM
@docs decodeN
@docs decodeNS
@docs decodeNULL
@docs decodeQueryResponse
@docs decodeS
@docs decodeSS
@docs encodeB
@docs encodeBOOL
@docs encodeBS
@docs encodeJsonEncoded
@docs encodeL
@docs encodeM
@docs encodeN
@docs encodeNS
@docs encodeNULL
@docs encodeS
@docs encodeSS
@docs fromBase64
@docs toBase64

See <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_AttributeValue.html>

-}

import AWS
import AWS.Types
import Base64
import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import Set exposing (Set)
import Task exposing (Task)
import Time


{-| A wrapper to differentiate from regular String
-}
type Base64String
    = Base64String String


{-| -}
toBase64 : String -> Result String Base64String
toBase64 string =
    Base64.decode string
        |> Result.map (always (Base64String string))


{-| Obtain the regular `String` content. Decode it yourself, e.g.
<https://package.elm-lang.org/packages/waratuman/elm-coder/3.0.1/Base64#decode>
-}
fromBase64 : Base64String -> String
fromBase64 (Base64String s) =
    s


{-|

    import Json.Decode

    toBase64 "aGVsbG8="
    |> Result.andThen (\v ->
        Json.Decode.decodeValue decodeB (encodeB v)
            |> Result.mapError Json.Decode.errorToString
    )
    --> toBase64 "aGVsbG8="

-}
decodeB : Json.Decode.Decoder Base64String
decodeB =
    Json.Decode.field "B" Json.Decode.string
        |> Json.Decode.map Base64String


{-|

    import Json.Decode

    Json.Decode.decodeValue decodeBOOL (encodeBOOL True)
    --> Ok True

-}
decodeBOOL : Json.Decode.Decoder Bool
decodeBOOL =
    Json.Decode.field "BOOL" Json.Decode.bool


{-|

    import Json.Decode

    toBase64 "aGVsbG8="
    |> Result.andThen (\v ->
        Json.Decode.decodeValue decodeBS (encodeBS [v])
            |> Result.mapError Json.Decode.errorToString
    )
    --> Result.map (\v -> [v]) (toBase64 "aGVsbG8=")

-}
decodeBS : Json.Decode.Decoder (List Base64String)
decodeBS =
    Json.Decode.field "BS" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.map (Set.fromList >> Set.toList)
        |> Json.Decode.map (List.map Base64String)


{-|

    import Json.Decode

    toBase64 "aGVsbG8="
    |> Result.andThen (\v ->
        Json.Decode.decodeValue (decodeL decodeB) (encodeL encodeB [v])
            |> Result.mapError Json.Decode.errorToString
    )
    --> Result.map (\v -> [v]) (toBase64 "aGVsbG8=")

    Json.Decode.decodeValue (decodeL decodeN) (encodeL encodeN [3.14, 42])
    --> Ok [3.14, 42]

-}
decodeL : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
decodeL decoder =
    Json.Decode.field "L" (Json.Decode.list decoder)


{-|

    import Json.Decode
    import Dict

    Json.Decode.decodeValue (decodeM decodeN) (encodeM encodeN (Dict.fromList [("hello", 3.14)]))
    --> Ok (Dict.fromList [("hello", 3.14)])

-}
decodeM : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeM decoder =
    Json.Decode.field "M" (Json.Decode.dict decoder)


{-|

    import Json.Decode

    Json.Decode.decodeValue decodeS (encodeS "Hello")
    --> Ok "Hello"

-}
decodeS : Json.Decode.Decoder String
decodeS =
    Json.Decode.field "S" Json.Decode.string


decodeFloatString : Json.Decode.Decoder Float
decodeFloatString =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case String.toFloat s of
                    Nothing ->
                        Json.Decode.fail ("Invalid number: " ++ s)

                    Just a ->
                        Json.Decode.succeed a
            )


{-|

    import Json.Decode

    Json.Decode.decodeValue decodeN (encodeN 3.14)
    --> Ok 3.14

-}
decodeN : Json.Decode.Decoder Float
decodeN =
    Json.Decode.field "N" decodeFloatString


{-|

    import Json.Decode
    import Set

    Json.Decode.decodeValue decodeNS (encodeNS (Set.fromList [1, 3, 5, 7]))
    --> Ok (Set.fromList [1, 3, 5, 7])

-}
decodeNS : Json.Decode.Decoder (Set Float)
decodeNS =
    Json.Decode.field "NS" (Json.Decode.list decodeFloatString)
        |> Json.Decode.map Set.fromList


{-|

    import Json.Decode

    Json.Decode.decodeValue decodeNULL (encodeNULL False)
    --> Ok False

-}
decodeNULL : Json.Decode.Decoder Bool
decodeNULL =
    Json.Decode.field "NULL" Json.Decode.bool


{-|

    import Json.Decode
    import Set

    Json.Decode.decodeValue decodeSS (encodeSS (Set.fromList ["hello", "world"]))
    --> Ok (Set.fromList ["hello", "world"])

-}
decodeSS : Json.Decode.Decoder (Set String)
decodeSS =
    Json.Decode.field "SS" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.map Set.fromList



-- ENCODERS


{-|

    import Json.Encode

    toBase64 "aGVsbG8="
    |> Result.map (\v -> Json.Encode.encode 0 (encodeB v))
    --> Ok """{"B":"aGVsbG8="}"""

-}
encodeB : Base64String -> Json.Encode.Value
encodeB (Base64String v) =
    Json.Encode.object [ ( "B", Json.Encode.string v ) ]


{-|

    import Json.Encode

    Json.Encode.encode 0 (encodeBOOL False)
    --> """{"BOOL":false}"""

-}
encodeBOOL : Bool -> Json.Encode.Value
encodeBOOL v =
    Json.Encode.object [ ( "BOOL", Json.Encode.bool v ) ]


{-|

    import Json.Encode

    toBase64 "aGVsbG8="
    |> Result.map (\v -> Json.Encode.encode 0 (encodeBS ([v])))
    --> Ok """{"BS":["aGVsbG8="]}"""

-}
encodeBS : List Base64String -> Json.Encode.Value
encodeBS list =
    Json.Encode.object
        [ ( "BS"
          , List.map (\(Base64String s) -> s) list
                |> Json.Encode.list Json.Encode.string
          )
        ]


{-|

    import Json.Encode

    Json.Encode.encode 0 (encodeL encodeN [1,3,5,7])
    --> """{"L":[{"N":"1"},{"N":"3"},{"N":"5"},{"N":"7"}]}"""

-}
encodeL : (a -> Json.Encode.Value) -> List a -> Json.Encode.Value
encodeL encoder list =
    Json.Encode.object [ ( "L", Json.Encode.list encoder list ) ]


{-|

    import Json.Encode
    import Dict

    Json.Encode.encode 0 (encodeM encodeN (Dict.fromList [("width", 800), ("height", 600)]))
    --> """{"M":{"height":{"N":"600"},"width":{"N":"800"}}}"""

-}
encodeM : (a -> Json.Encode.Value) -> Dict String a -> Json.Encode.Value
encodeM encoder dict =
    Json.Encode.object [ ( "M", Json.Encode.dict identity encoder dict ) ]


{-|

    import Json.Encode

    Json.Encode.encode 0 (encodeS "Hello")
    --> """{"S":"Hello"}"""

-}
encodeS : String -> Json.Encode.Value
encodeS v =
    Json.Encode.object [ ( "S", Json.Encode.string v ) ]


{-|

    import Json.Encode
    import Set

    Json.Encode.encode 0 (encodeNS (Set.fromList [1,3,5,7]))
    --> """{"NS":["1","3","5","7"]}"""

-}
encodeNS : Set Float -> Json.Encode.Value
encodeNS floatSet =
    Json.Encode.object
        [ ( "NS"
          , Set.map String.fromFloat floatSet
                |> Json.Encode.set Json.Encode.string
          )
        ]


{-|

    import Json.Encode

    Json.Encode.encode 0 (encodeN 3.14)
    --> """{"N":"3.14"}"""

-}
encodeN : Float -> Json.Encode.Value
encodeN v =
    Json.Encode.object [ ( "N", Json.Encode.string (String.fromFloat v) ) ]


{-|

    import Json.Encode

    Json.Encode.encode 0 (encodeNULL True)
    --> """{"NULL":true}"""

-}
encodeNULL : Bool -> Json.Encode.Value
encodeNULL v =
    Json.Encode.object [ ( "NULL", Json.Encode.bool v ) ]


{-|

    import Json.Encode
    import Set

    Json.Encode.encode 0 (encodeSS (Set.fromList ["hello", "world"]))
    --> """{"SS":["hello","world"]}"""

-}
encodeSS : Set String -> Json.Encode.Value
encodeSS stringSet =
    Json.Encode.object
        [ ( "SS"
          , Json.Encode.set Json.Encode.string stringSet
          )
        ]


{-| Structure of DynamoDB response
-}
type alias QueryResponse a =
    { count : Int
    , items : List a
    , lastEvaluatedKey : Json.Encode.Value
    }


{-| -}
decodeQueryResponse : Json.Decode.Decoder a -> Json.Decode.Decoder (QueryResponse a)
decodeQueryResponse decoder =
    Json.Decode.map3 QueryResponse
        (Json.Decode.field "Count" Json.Decode.int)
        (Json.Decode.field "Items" (Json.Decode.list decoder))
        (Json.Decode.oneOf
            [ Json.Decode.field "LastEvaluatedKey" Json.Decode.value
            , Json.Decode.succeed Json.Encode.null
            ]
        )


{-| -}
encodeQueryResponse : (a -> Json.Encode.Value) -> QueryResponse a -> Json.Encode.Value
encodeQueryResponse encoder v =
    Json.Encode.object
        [ ( "Count", Json.Encode.int v.count )
        , ( "Items", Json.Encode.list encoder v.items )
        , ( "LastEvaluatedKey", v.lastEvaluatedKey )
        ]


{-| wrapper type to store data as a String in a dynamodb column
-}
type JsonEncoded a
    = JsonEncoded a


{-| -}
encodeJsonEncoded : (a -> Json.Encode.Value) -> JsonEncoded a -> Json.Encode.Value
encodeJsonEncoded encoder (JsonEncoded a) =
    Json.Encode.encode 0 (encoder a)
        |> encodeS


{-| -}
decodeJsonEncoded : Json.Decode.Decoder a -> Json.Decode.Decoder (JsonEncoded a)
decodeJsonEncoded decoder =
    decodeS
        |> Json.Decode.andThen
            (\s ->
                case Json.Decode.decodeString decoder s of
                    Err err ->
                        Json.Decode.fail (Json.Decode.errorToString err)

                    Ok a ->
                        Json.Decode.succeed (JsonEncoded a)
            )
