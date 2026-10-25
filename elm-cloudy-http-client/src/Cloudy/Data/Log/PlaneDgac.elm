module Cloudy.Data.Log.PlaneDgac exposing (Log, Patch, decoder, encodeCreate, encodePatch, encodeReplace)

{-| Generated from Cloudy OpenAPI. Do not edit by hand; run scripts/generate\_elm.py. |
-}

import Cloudy.Data.AttachedFile as AttachedFile
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Log =
    { logbook_id : Int
    , id : Int
    , visible : Bool
    , deleted : Bool
    , flight_date : String
    , updated_at : String
    , created_at : String
    , optional_filter : String
    , aircraft_type : String
    , aircraft_registration : String
    , function_on_board : String
    , flight_nature : String
    , crew_day_minutes : Int
    , crew_night_minutes : Int
    , single_engine_day_dual_minutes : Int
    , single_engine_day_captain_minutes : Int
    , single_engine_night_dual_minutes : Int
    , single_engine_night_captain_minutes : Int
    , multi_engine_day_dual_minutes : Int
    , multi_engine_day_captain_minutes : Int
    , multi_engine_day_co_pilot_minutes : Int
    , multi_engine_night_dual_minutes : Int
    , multi_engine_night_captain_minutes : Int
    , multi_engine_night_co_pilot_minutes : Int
    , instrument_day_dual_minutes : Int
    , instrument_pilot_minutes : Int
    , simulator_minutes : Int
    , ifr : Int
    , observations : String
    , comments : String
    , takeoff_count : Int
    , landing_count : Int
    , departure_airport : String
    , arrival_airport : String
    , passengers : List String
    , files : List AttachedFile.AttachedFile
    }


type alias Patch =
    { flight_date : Maybe String
    , optional_filter : Maybe String
    , aircraft_type : Maybe String
    , aircraft_registration : Maybe String
    , function_on_board : Maybe String
    , flight_nature : Maybe String
    , crew_day_minutes : Maybe Int
    , crew_night_minutes : Maybe Int
    , single_engine_day_dual_minutes : Maybe Int
    , single_engine_day_captain_minutes : Maybe Int
    , single_engine_night_dual_minutes : Maybe Int
    , single_engine_night_captain_minutes : Maybe Int
    , multi_engine_day_dual_minutes : Maybe Int
    , multi_engine_day_captain_minutes : Maybe Int
    , multi_engine_day_co_pilot_minutes : Maybe Int
    , multi_engine_night_dual_minutes : Maybe Int
    , multi_engine_night_captain_minutes : Maybe Int
    , multi_engine_night_co_pilot_minutes : Maybe Int
    , instrument_day_dual_minutes : Maybe Int
    , instrument_pilot_minutes : Maybe Int
    , simulator_minutes : Maybe Int
    , ifr : Maybe Int
    , observations : Maybe String
    , comments : Maybe String
    , takeoff_count : Maybe Int
    , landing_count : Maybe Int
    , departure_airport : Maybe String
    , arrival_airport : Maybe String
    , passengers : Maybe (List String)
    }


decoder : Decoder Log
decoder =
    Decode.succeed Log
        |> required "logbook_id" Decode.int
        |> required "id" Decode.int
        |> required "visible" Decode.bool
        |> required "deleted" Decode.bool
        |> required "flight_date" Decode.string
        |> required "updated_at" Decode.string
        |> required "created_at" Decode.string
        |> required "optional_filter" Decode.string
        |> required "aircraft_type" Decode.string
        |> required "aircraft_registration" Decode.string
        |> required "function_on_board" Decode.string
        |> required "flight_nature" Decode.string
        |> required "crew_day_minutes" Decode.int
        |> required "crew_night_minutes" Decode.int
        |> required "single_engine_day_dual_minutes" Decode.int
        |> required "single_engine_day_captain_minutes" Decode.int
        |> required "single_engine_night_dual_minutes" Decode.int
        |> required "single_engine_night_captain_minutes" Decode.int
        |> required "multi_engine_day_dual_minutes" Decode.int
        |> required "multi_engine_day_captain_minutes" Decode.int
        |> required "multi_engine_day_co_pilot_minutes" Decode.int
        |> required "multi_engine_night_dual_minutes" Decode.int
        |> required "multi_engine_night_captain_minutes" Decode.int
        |> required "multi_engine_night_co_pilot_minutes" Decode.int
        |> required "instrument_day_dual_minutes" Decode.int
        |> required "instrument_pilot_minutes" Decode.int
        |> required "simulator_minutes" Decode.int
        |> required "ifr" Decode.int
        |> required "observations" Decode.string
        |> required "comments" Decode.string
        |> required "takeoff_count" Decode.int
        |> required "landing_count" Decode.int
        |> required "departure_airport" Decode.string
        |> required "arrival_airport" Decode.string
        |> required "passengers" (Decode.list Decode.string)
        |> optional "files" (Decode.list AttachedFile.decoder) []


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field dec builder =
    Decode.map2 (\v f -> f v) (Decode.field field dec) builder


optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional field dec default builder =
    Decode.map2
        (\mf f ->
            case mf of
                Just v ->
                    f v

                Nothing ->
                    f default
        )
        (Decode.maybe (Decode.field field dec))
        builder


encodeCreate : Log -> Value
encodeCreate record =
    encodeReplace record


encodeReplace : Log -> Value
encodeReplace record =
    Encode.object
        [ ( "flight_date", Encode.string record.flight_date )
        , ( "optional_filter", Encode.string record.optional_filter )
        , ( "aircraft_type", Encode.string record.aircraft_type )
        , ( "aircraft_registration", Encode.string record.aircraft_registration )
        , ( "function_on_board", Encode.string record.function_on_board )
        , ( "flight_nature", Encode.string record.flight_nature )
        , ( "crew_day_minutes", Encode.int record.crew_day_minutes )
        , ( "crew_night_minutes", Encode.int record.crew_night_minutes )
        , ( "single_engine_day_dual_minutes", Encode.int record.single_engine_day_dual_minutes )
        , ( "single_engine_day_captain_minutes", Encode.int record.single_engine_day_captain_minutes )
        , ( "single_engine_night_dual_minutes", Encode.int record.single_engine_night_dual_minutes )
        , ( "single_engine_night_captain_minutes", Encode.int record.single_engine_night_captain_minutes )
        , ( "multi_engine_day_dual_minutes", Encode.int record.multi_engine_day_dual_minutes )
        , ( "multi_engine_day_captain_minutes", Encode.int record.multi_engine_day_captain_minutes )
        , ( "multi_engine_day_co_pilot_minutes", Encode.int record.multi_engine_day_co_pilot_minutes )
        , ( "multi_engine_night_dual_minutes", Encode.int record.multi_engine_night_dual_minutes )
        , ( "multi_engine_night_captain_minutes", Encode.int record.multi_engine_night_captain_minutes )
        , ( "multi_engine_night_co_pilot_minutes", Encode.int record.multi_engine_night_co_pilot_minutes )
        , ( "instrument_day_dual_minutes", Encode.int record.instrument_day_dual_minutes )
        , ( "instrument_pilot_minutes", Encode.int record.instrument_pilot_minutes )
        , ( "simulator_minutes", Encode.int record.simulator_minutes )
        , ( "ifr", Encode.int record.ifr )
        , ( "observations", Encode.string record.observations )
        , ( "comments", Encode.string record.comments )
        , ( "takeoff_count", Encode.int record.takeoff_count )
        , ( "landing_count", Encode.int record.landing_count )
        , ( "departure_airport", Encode.string record.departure_airport )
        , ( "arrival_airport", Encode.string record.arrival_airport )
        , ( "passengers", Encode.list Encode.string record.passengers )
        ]


encodePatch : Patch -> Value
encodePatch record =
    Encode.object
        (List.filterMap identity
            [ maybeMap record.flight_date (\v -> ( "flight_date", Encode.string v ))
            , maybeMap record.optional_filter (\v -> ( "optional_filter", Encode.string v ))
            , maybeMap record.aircraft_type (\v -> ( "aircraft_type", Encode.string v ))
            , maybeMap record.aircraft_registration (\v -> ( "aircraft_registration", Encode.string v ))
            , maybeMap record.function_on_board (\v -> ( "function_on_board", Encode.string v ))
            , maybeMap record.flight_nature (\v -> ( "flight_nature", Encode.string v ))
            , maybeMap record.crew_day_minutes (\v -> ( "crew_day_minutes", Encode.int v ))
            , maybeMap record.crew_night_minutes (\v -> ( "crew_night_minutes", Encode.int v ))
            , maybeMap record.single_engine_day_dual_minutes (\v -> ( "single_engine_day_dual_minutes", Encode.int v ))
            , maybeMap record.single_engine_day_captain_minutes (\v -> ( "single_engine_day_captain_minutes", Encode.int v ))
            , maybeMap record.single_engine_night_dual_minutes (\v -> ( "single_engine_night_dual_minutes", Encode.int v ))
            , maybeMap record.single_engine_night_captain_minutes (\v -> ( "single_engine_night_captain_minutes", Encode.int v ))
            , maybeMap record.multi_engine_day_dual_minutes (\v -> ( "multi_engine_day_dual_minutes", Encode.int v ))
            , maybeMap record.multi_engine_day_captain_minutes (\v -> ( "multi_engine_day_captain_minutes", Encode.int v ))
            , maybeMap record.multi_engine_day_co_pilot_minutes (\v -> ( "multi_engine_day_co_pilot_minutes", Encode.int v ))
            , maybeMap record.multi_engine_night_dual_minutes (\v -> ( "multi_engine_night_dual_minutes", Encode.int v ))
            , maybeMap record.multi_engine_night_captain_minutes (\v -> ( "multi_engine_night_captain_minutes", Encode.int v ))
            , maybeMap record.multi_engine_night_co_pilot_minutes (\v -> ( "multi_engine_night_co_pilot_minutes", Encode.int v ))
            , maybeMap record.instrument_day_dual_minutes (\v -> ( "instrument_day_dual_minutes", Encode.int v ))
            , maybeMap record.instrument_pilot_minutes (\v -> ( "instrument_pilot_minutes", Encode.int v ))
            , maybeMap record.simulator_minutes (\v -> ( "simulator_minutes", Encode.int v ))
            , maybeMap record.ifr (\v -> ( "ifr", Encode.int v ))
            , maybeMap record.observations (\v -> ( "observations", Encode.string v ))
            , maybeMap record.comments (\v -> ( "comments", Encode.string v ))
            , maybeMap record.takeoff_count (\v -> ( "takeoff_count", Encode.int v ))
            , maybeMap record.landing_count (\v -> ( "landing_count", Encode.int v ))
            , maybeMap record.departure_airport (\v -> ( "departure_airport", Encode.string v ))
            , maybeMap record.arrival_airport (\v -> ( "arrival_airport", Encode.string v ))
            , maybeMap record.passengers (\v -> ( "passengers", Encode.list Encode.string v ))
            ]
        )


maybeMap : Maybe a -> (a -> b) -> Maybe b
maybeMap m f =
    case m of
        Just v ->
            Just (f v)

        Nothing ->
            Nothing
