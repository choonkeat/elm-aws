# elm-aws

Helper functions to create signed http requests to AWS, e.g. DynamoDB or IAM

``` elm
-- Setup the request


stringBody =
    Json.Encode.object
        [ ( "TableName", Json.Encode.string "Example" )
        , ( "Key"
          , Json.Encode.object
                [ ( "primaryKey", AWS.DynamoDB.encodeS "uniqueValue" )
                ]
          )
        ]
        |> Json.Encode.encode 0

rawRequest =
    { method = "POST"
    , headers =
        [ AWS.awsTargetHeader AWS.DynamoDB_GetItem
        ]
    , query = []
    , stringBody = stringBody
    , resolver = httpResolver
    , timeout = httpTimeout
    }


-- If AWS.signRequest is successful, return a Task


httpTask =
    case AWS.signRequest config now rawRequest of
        Err err ->
            -- unable to create request

        Ok signedRequest ->
            Http.task signedRequest
```
