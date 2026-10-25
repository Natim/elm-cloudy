module Cloudy.Data.Report.PlaneFcl1080 exposing (Report, decoder, encodeReplace)

{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate\_elm.py. |
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Report =
    { take_off_day : Int
    , take_off_night : Int
    , landing_day : Int
    , landing_night : Int
    , log_id : String
    , single_pilot_time : String
    , multi_pilot_time : String
    , total_time_flight : String
    , op_time_night : String
    , op_time_ifr : String
    , pilot_time_in_cmd : String
    , co_pilot_minutes : String
    , pilot_time_dual : String
    , pilot_time_inst : String
    , synthetic_time : String
    }


decoder : Decoder Report
decoder =
    Decode.succeed Report
        |> required "take_off_day" Decode.int
        |> required "take_off_night" Decode.int
        |> required "landing_day" Decode.int
        |> required "landing_night" Decode.int
        |> required "log_id" Decode.string
        |> required "single_pilot_time" Decode.string
        |> required "multi_pilot_time" Decode.string
        |> required "total_time_flight" Decode.string
        |> required "op_time_night" Decode.string
        |> required "op_time_ifr" Decode.string
        |> required "pilot_time_in_cmd" Decode.string
        |> required "co_pilot_minutes" Decode.string
        |> required "pilot_time_dual" Decode.string
        |> required "pilot_time_inst" Decode.string
        |> required "synthetic_time" Decode.string


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field dec builder =
    Decode.map2 (\v f -> f v) (Decode.field field dec) builder


encodeReplace : Report -> Value
encodeReplace record =
    Encode.object
        [ ( "take_off_day", Encode.int record.take_off_day )
        , ( "take_off_night", Encode.int record.take_off_night )
        , ( "landing_day", Encode.int record.landing_day )
        , ( "landing_night", Encode.int record.landing_night )
        , ( "log_id", Encode.string record.log_id )
        , ( "single_pilot_time", Encode.string record.single_pilot_time )
        , ( "multi_pilot_time", Encode.string record.multi_pilot_time )
        , ( "total_time_flight", Encode.string record.total_time_flight )
        , ( "op_time_night", Encode.string record.op_time_night )
        , ( "op_time_ifr", Encode.string record.op_time_ifr )
        , ( "pilot_time_in_cmd", Encode.string record.pilot_time_in_cmd )
        , ( "co_pilot_minutes", Encode.string record.co_pilot_minutes )
        , ( "pilot_time_dual", Encode.string record.pilot_time_dual )
        , ( "pilot_time_inst", Encode.string record.pilot_time_inst )
        , ( "synthetic_time", Encode.string record.synthetic_time )
        ]
