module Cloudy.Data.ApiErrorBody exposing (ApiErrorBody, FieldError, decoder)

import Dict
import Json.Decode as Decode exposing (Decoder)


type alias FieldError =
    { message : String
    , code : String
    }


type alias ApiErrorBody =
    { detail : String
    , code : String
    , errors : Dict.Dict String (List FieldError)
    }


fieldErrorDecoder : Decoder FieldError
fieldErrorDecoder =
    Decode.map2 FieldError
        (Decode.field "message" Decode.string)
        (Decode.field "code" Decode.string)


decoder : Decoder ApiErrorBody
decoder =
    Decode.map3 ApiErrorBody
        (Decode.field "detail" Decode.string)
        (Decode.field "code" Decode.string)
        (Decode.oneOf
            [ Decode.field "errors" (Decode.dict (Decode.list fieldErrorDecoder))
            , Decode.succeed Dict.empty
            ]
        )
