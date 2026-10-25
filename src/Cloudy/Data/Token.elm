module Cloudy.Data.Token exposing (TokenRequest, TokenResponse, decoder, encodeRequest)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias TokenResponse =
    { accessToken : String
    , tokenType : String
    , expiresIn : Int
    , userId : Int
    }


type alias TokenRequest =
    { email : String
    , password : String
    }


decoder : Decoder TokenResponse
decoder =
    Decode.map4 TokenResponse
        (Decode.field "access_token" Decode.string)
        (Decode.field "token_type" Decode.string)
        (Decode.field "expires_in" Decode.int)
        (Decode.field "user_id" Decode.int)


encodeRequest : TokenRequest -> Value
encodeRequest payload =
    Encode.object
        [ ( "email", Encode.string payload.email )
        , ( "password", Encode.string payload.password )
        ]
