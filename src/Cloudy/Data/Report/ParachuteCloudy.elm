module Cloudy.Data.Report.ParachuteCloudy exposing (Report, decoder, encodeReplace)

{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate\_elm.py. |
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Report =
    { jump_count : Int
    , log_id : String
    }


decoder : Decoder Report
decoder =
    Decode.succeed Report
        |> required "jump_count" Decode.int
        |> required "log_id" Decode.string


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field dec builder =
    Decode.map2 (\v f -> f v) (Decode.field field dec) builder


encodeReplace : Report -> Value
encodeReplace record =
    Encode.object
        [ ( "jump_count", Encode.int record.jump_count )
        , ( "log_id", Encode.string record.log_id )
        ]
