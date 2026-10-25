module Cloudy.Data.Report.GliderDgac exposing (Report, decoder, encodeReplace)

{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate\_elm.py. |
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Report =
    { ifr : Int
    , takeoff_count : Int
    , landing_count : Int
    , log_id : String
    , crew_day_minutes : String
    , crew_night_minutes : String
    , single_engine_day_dual_minutes : String
    , single_engine_day_captain_minutes : String
    , single_engine_night_dual_minutes : String
    , single_engine_night_captain_minutes : String
    , multi_engine_day_dual_minutes : String
    , multi_engine_day_captain_minutes : String
    , multi_engine_day_co_pilot_minutes : String
    , multi_engine_night_dual_minutes : String
    , multi_engine_night_captain_minutes : String
    , multi_engine_night_co_pilot_minutes : String
    , instrument_day_dual_minutes : String
    , instrument_pilot_minutes : String
    , simulator_minutes : String
    }


decoder : Decoder Report
decoder =
    Decode.succeed Report
        |> required "ifr" Decode.int
        |> required "takeoff_count" Decode.int
        |> required "landing_count" Decode.int
        |> required "log_id" Decode.string
        |> required "crew_day_minutes" Decode.string
        |> required "crew_night_minutes" Decode.string
        |> required "single_engine_day_dual_minutes" Decode.string
        |> required "single_engine_day_captain_minutes" Decode.string
        |> required "single_engine_night_dual_minutes" Decode.string
        |> required "single_engine_night_captain_minutes" Decode.string
        |> required "multi_engine_day_dual_minutes" Decode.string
        |> required "multi_engine_day_captain_minutes" Decode.string
        |> required "multi_engine_day_co_pilot_minutes" Decode.string
        |> required "multi_engine_night_dual_minutes" Decode.string
        |> required "multi_engine_night_captain_minutes" Decode.string
        |> required "multi_engine_night_co_pilot_minutes" Decode.string
        |> required "instrument_day_dual_minutes" Decode.string
        |> required "instrument_pilot_minutes" Decode.string
        |> required "simulator_minutes" Decode.string


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field dec builder =
    Decode.map2 (\v f -> f v) (Decode.field field dec) builder


encodeReplace : Report -> Value
encodeReplace record =
    Encode.object
        [ ( "ifr", Encode.int record.ifr )
        , ( "takeoff_count", Encode.int record.takeoff_count )
        , ( "landing_count", Encode.int record.landing_count )
        , ( "log_id", Encode.string record.log_id )
        , ( "crew_day_minutes", Encode.string record.crew_day_minutes )
        , ( "crew_night_minutes", Encode.string record.crew_night_minutes )
        , ( "single_engine_day_dual_minutes", Encode.string record.single_engine_day_dual_minutes )
        , ( "single_engine_day_captain_minutes", Encode.string record.single_engine_day_captain_minutes )
        , ( "single_engine_night_dual_minutes", Encode.string record.single_engine_night_dual_minutes )
        , ( "single_engine_night_captain_minutes", Encode.string record.single_engine_night_captain_minutes )
        , ( "multi_engine_day_dual_minutes", Encode.string record.multi_engine_day_dual_minutes )
        , ( "multi_engine_day_captain_minutes", Encode.string record.multi_engine_day_captain_minutes )
        , ( "multi_engine_day_co_pilot_minutes", Encode.string record.multi_engine_day_co_pilot_minutes )
        , ( "multi_engine_night_dual_minutes", Encode.string record.multi_engine_night_dual_minutes )
        , ( "multi_engine_night_captain_minutes", Encode.string record.multi_engine_night_captain_minutes )
        , ( "multi_engine_night_co_pilot_minutes", Encode.string record.multi_engine_night_co_pilot_minutes )
        , ( "instrument_day_dual_minutes", Encode.string record.instrument_day_dual_minutes )
        , ( "instrument_pilot_minutes", Encode.string record.instrument_pilot_minutes )
        , ( "simulator_minutes", Encode.string record.simulator_minutes )
        ]
