module Cloudy.Data.Report.UlmFfplum exposing (Report, decoder, encodeReplace)

{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate\_elm.py. |
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Report =
    { takeoff_count : Int
    , landing_count : Int
    , log_id : String
    , dual_minutes : String
    , pilot_in_command_minutes : String
    }


decoder : Decoder Report
decoder =
    Decode.succeed Report
        |> required "takeoff_count" Decode.int
        |> required "landing_count" Decode.int
        |> required "log_id" Decode.string
        |> required "dual_minutes" Decode.string
        |> required "pilot_in_command_minutes" Decode.string


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field dec builder =
    Decode.map2 (\v f -> f v) (Decode.field field dec) builder


encodeReplace : Report -> Value
encodeReplace record =
    Encode.object
        [ ( "takeoff_count", Encode.int record.takeoff_count )
        , ( "landing_count", Encode.int record.landing_count )
        , ( "log_id", Encode.string record.log_id )
        , ( "dual_minutes", Encode.string record.dual_minutes )
        , ( "pilot_in_command_minutes", Encode.string record.pilot_in_command_minutes )
        ]
