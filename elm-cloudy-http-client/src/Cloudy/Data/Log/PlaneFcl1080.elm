module Cloudy.Data.Log.PlaneFcl1080 exposing (Log, Patch, decoder, encodeCreate, encodePatch, encodeReplace)

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
    , departure_place : String
    , departure_time : String
    , arrival_place : String
    , arrival_time : String
    , aircraft_type : String
    , aircraft_registration : String
    , single_pilot_se : Bool
    , single_pilot_me : Bool
    , single_pilot_time : Int
    , multi_pilot_time : Int
    , total_time_flight : Int
    , pilot_in_command_name : String
    , take_off_day : Int
    , take_off_night : Int
    , landing_day : Int
    , landing_night : Int
    , op_time_night : Int
    , op_time_ifr : Int
    , pilot_time_in_cmd : Int
    , co_pilot_minutes : Int
    , pilot_time_dual : Int
    , pilot_time_inst : Int
    , synthetic_date : String
    , synthetic_type : String
    , synthetic_time : Int
    , observations : String
    , comments : String
    , passengers : List String
    , files : List AttachedFile.AttachedFile
    }


type alias Patch =
    { flight_date : Maybe String
    , optional_filter : Maybe String
    , departure_place : Maybe String
    , departure_time : Maybe String
    , arrival_place : Maybe String
    , arrival_time : Maybe String
    , aircraft_type : Maybe String
    , aircraft_registration : Maybe String
    , single_pilot_se : Maybe Bool
    , single_pilot_me : Maybe Bool
    , single_pilot_time : Maybe Int
    , multi_pilot_time : Maybe Int
    , total_time_flight : Maybe Int
    , pilot_in_command_name : Maybe String
    , take_off_day : Maybe Int
    , take_off_night : Maybe Int
    , landing_day : Maybe Int
    , landing_night : Maybe Int
    , op_time_night : Maybe Int
    , op_time_ifr : Maybe Int
    , pilot_time_in_cmd : Maybe Int
    , co_pilot_minutes : Maybe Int
    , pilot_time_dual : Maybe Int
    , pilot_time_inst : Maybe Int
    , synthetic_date : Maybe String
    , synthetic_type : Maybe String
    , synthetic_time : Maybe Int
    , observations : Maybe String
    , comments : Maybe String
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
        |> required "departure_place" Decode.string
        |> required "departure_time" Decode.string
        |> required "arrival_place" Decode.string
        |> required "arrival_time" Decode.string
        |> required "aircraft_type" Decode.string
        |> required "aircraft_registration" Decode.string
        |> required "single_pilot_se" Decode.bool
        |> required "single_pilot_me" Decode.bool
        |> required "single_pilot_time" Decode.int
        |> required "multi_pilot_time" Decode.int
        |> required "total_time_flight" Decode.int
        |> required "pilot_in_command_name" Decode.string
        |> required "take_off_day" Decode.int
        |> required "take_off_night" Decode.int
        |> required "landing_day" Decode.int
        |> required "landing_night" Decode.int
        |> required "op_time_night" Decode.int
        |> required "op_time_ifr" Decode.int
        |> required "pilot_time_in_cmd" Decode.int
        |> required "co_pilot_minutes" Decode.int
        |> required "pilot_time_dual" Decode.int
        |> required "pilot_time_inst" Decode.int
        |> optional "synthetic_date" Decode.string ""
        |> required "synthetic_type" Decode.string
        |> required "synthetic_time" Decode.int
        |> required "observations" Decode.string
        |> required "comments" Decode.string
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
        , ( "departure_place", Encode.string record.departure_place )
        , ( "departure_time", Encode.string record.departure_time )
        , ( "arrival_place", Encode.string record.arrival_place )
        , ( "arrival_time", Encode.string record.arrival_time )
        , ( "aircraft_type", Encode.string record.aircraft_type )
        , ( "aircraft_registration", Encode.string record.aircraft_registration )
        , ( "single_pilot_se", Encode.bool record.single_pilot_se )
        , ( "single_pilot_me", Encode.bool record.single_pilot_me )
        , ( "single_pilot_time", Encode.int record.single_pilot_time )
        , ( "multi_pilot_time", Encode.int record.multi_pilot_time )
        , ( "total_time_flight", Encode.int record.total_time_flight )
        , ( "pilot_in_command_name", Encode.string record.pilot_in_command_name )
        , ( "take_off_day", Encode.int record.take_off_day )
        , ( "take_off_night", Encode.int record.take_off_night )
        , ( "landing_day", Encode.int record.landing_day )
        , ( "landing_night", Encode.int record.landing_night )
        , ( "op_time_night", Encode.int record.op_time_night )
        , ( "op_time_ifr", Encode.int record.op_time_ifr )
        , ( "pilot_time_in_cmd", Encode.int record.pilot_time_in_cmd )
        , ( "co_pilot_minutes", Encode.int record.co_pilot_minutes )
        , ( "pilot_time_dual", Encode.int record.pilot_time_dual )
        , ( "pilot_time_inst", Encode.int record.pilot_time_inst )
        , ( "synthetic_date", Encode.string record.synthetic_date )
        , ( "synthetic_type", Encode.string record.synthetic_type )
        , ( "synthetic_time", Encode.int record.synthetic_time )
        , ( "observations", Encode.string record.observations )
        , ( "comments", Encode.string record.comments )
        , ( "passengers", Encode.list Encode.string record.passengers )
        ]


encodePatch : Patch -> Value
encodePatch record =
    Encode.object
        (List.filterMap identity
            [ maybeMap record.flight_date (\v -> ( "flight_date", Encode.string v ))
            , maybeMap record.optional_filter (\v -> ( "optional_filter", Encode.string v ))
            , maybeMap record.departure_place (\v -> ( "departure_place", Encode.string v ))
            , maybeMap record.departure_time (\v -> ( "departure_time", Encode.string v ))
            , maybeMap record.arrival_place (\v -> ( "arrival_place", Encode.string v ))
            , maybeMap record.arrival_time (\v -> ( "arrival_time", Encode.string v ))
            , maybeMap record.aircraft_type (\v -> ( "aircraft_type", Encode.string v ))
            , maybeMap record.aircraft_registration (\v -> ( "aircraft_registration", Encode.string v ))
            , maybeMap record.single_pilot_se (\v -> ( "single_pilot_se", Encode.bool v ))
            , maybeMap record.single_pilot_me (\v -> ( "single_pilot_me", Encode.bool v ))
            , maybeMap record.single_pilot_time (\v -> ( "single_pilot_time", Encode.int v ))
            , maybeMap record.multi_pilot_time (\v -> ( "multi_pilot_time", Encode.int v ))
            , maybeMap record.total_time_flight (\v -> ( "total_time_flight", Encode.int v ))
            , maybeMap record.pilot_in_command_name (\v -> ( "pilot_in_command_name", Encode.string v ))
            , maybeMap record.take_off_day (\v -> ( "take_off_day", Encode.int v ))
            , maybeMap record.take_off_night (\v -> ( "take_off_night", Encode.int v ))
            , maybeMap record.landing_day (\v -> ( "landing_day", Encode.int v ))
            , maybeMap record.landing_night (\v -> ( "landing_night", Encode.int v ))
            , maybeMap record.op_time_night (\v -> ( "op_time_night", Encode.int v ))
            , maybeMap record.op_time_ifr (\v -> ( "op_time_ifr", Encode.int v ))
            , maybeMap record.pilot_time_in_cmd (\v -> ( "pilot_time_in_cmd", Encode.int v ))
            , maybeMap record.co_pilot_minutes (\v -> ( "co_pilot_minutes", Encode.int v ))
            , maybeMap record.pilot_time_dual (\v -> ( "pilot_time_dual", Encode.int v ))
            , maybeMap record.pilot_time_inst (\v -> ( "pilot_time_inst", Encode.int v ))
            , maybeMap record.synthetic_date (\v -> ( "synthetic_date", Encode.string v ))
            , maybeMap record.synthetic_type (\v -> ( "synthetic_type", Encode.string v ))
            , maybeMap record.synthetic_time (\v -> ( "synthetic_time", Encode.int v ))
            , maybeMap record.observations (\v -> ( "observations", Encode.string v ))
            , maybeMap record.comments (\v -> ( "comments", Encode.string v ))
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
