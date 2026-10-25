module Cloudy.Data.Log.ParachuteCloudy exposing (Log, Patch, decoder, encodeCreate, encodePatch, encodeReplace)

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
    , place : String
    , aircraft : String
    , altitude : Int
    , altitude_open : Int
    , canopy : String
    , bag : String
    , perf_exercise : String
    , freefall_training : String
    , canopy_piloting : String
    , comments : String
    , falling_time : Int
    , files : List AttachedFile.AttachedFile
    }


type alias Patch =
    { flight_date : Maybe String
    , optional_filter : Maybe String
    , place : Maybe String
    , aircraft : Maybe String
    , altitude : Maybe Int
    , altitude_open : Maybe Int
    , canopy : Maybe String
    , bag : Maybe String
    , perf_exercise : Maybe String
    , freefall_training : Maybe String
    , canopy_piloting : Maybe String
    , comments : Maybe String
    , falling_time : Maybe Int
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
        |> required "place" Decode.string
        |> required "aircraft" Decode.string
        |> required "altitude" Decode.int
        |> required "altitude_open" Decode.int
        |> required "canopy" Decode.string
        |> required "bag" Decode.string
        |> required "perf_exercise" Decode.string
        |> required "freefall_training" Decode.string
        |> required "canopy_piloting" Decode.string
        |> required "comments" Decode.string
        |> required "falling_time" Decode.int
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
        , ( "place", Encode.string record.place )
        , ( "aircraft", Encode.string record.aircraft )
        , ( "altitude", Encode.int record.altitude )
        , ( "altitude_open", Encode.int record.altitude_open )
        , ( "canopy", Encode.string record.canopy )
        , ( "bag", Encode.string record.bag )
        , ( "perf_exercise", Encode.string record.perf_exercise )
        , ( "freefall_training", Encode.string record.freefall_training )
        , ( "canopy_piloting", Encode.string record.canopy_piloting )
        , ( "comments", Encode.string record.comments )
        , ( "falling_time", Encode.int record.falling_time )
        ]


encodePatch : Patch -> Value
encodePatch record =
    Encode.object
        (List.filterMap identity
            [ maybeMap record.flight_date (\v -> ( "flight_date", Encode.string v ))
            , maybeMap record.optional_filter (\v -> ( "optional_filter", Encode.string v ))
            , maybeMap record.place (\v -> ( "place", Encode.string v ))
            , maybeMap record.aircraft (\v -> ( "aircraft", Encode.string v ))
            , maybeMap record.altitude (\v -> ( "altitude", Encode.int v ))
            , maybeMap record.altitude_open (\v -> ( "altitude_open", Encode.int v ))
            , maybeMap record.canopy (\v -> ( "canopy", Encode.string v ))
            , maybeMap record.bag (\v -> ( "bag", Encode.string v ))
            , maybeMap record.perf_exercise (\v -> ( "perf_exercise", Encode.string v ))
            , maybeMap record.freefall_training (\v -> ( "freefall_training", Encode.string v ))
            , maybeMap record.canopy_piloting (\v -> ( "canopy_piloting", Encode.string v ))
            , maybeMap record.comments (\v -> ( "comments", Encode.string v ))
            , maybeMap record.falling_time (\v -> ( "falling_time", Encode.int v ))
            ]
        )


maybeMap : Maybe a -> (a -> b) -> Maybe b
maybeMap m f =
    case m of
        Just v ->
            Just (f v)

        Nothing ->
            Nothing
