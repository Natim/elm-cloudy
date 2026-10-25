module Cloudy.Data.Log.BalloonDgac exposing (Log, Patch, decoder, encodeCreate, encodePatch, encodeReplace)

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
    , ballon_type : String
    , ballon_imma : String
    , dual_minutes : Int
    , pilot_in_command_minutes : Int
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
    , ballon_type : Maybe String
    , ballon_imma : Maybe String
    , dual_minutes : Maybe Int
    , pilot_in_command_minutes : Maybe Int
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
        |> required "ballon_type" Decode.string
        |> required "ballon_imma" Decode.string
        |> required "dual_minutes" Decode.int
        |> required "pilot_in_command_minutes" Decode.int
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
        , ( "ballon_type", Encode.string record.ballon_type )
        , ( "ballon_imma", Encode.string record.ballon_imma )
        , ( "dual_minutes", Encode.int record.dual_minutes )
        , ( "pilot_in_command_minutes", Encode.int record.pilot_in_command_minutes )
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
            , maybeMap record.ballon_type (\v -> ( "ballon_type", Encode.string v ))
            , maybeMap record.ballon_imma (\v -> ( "ballon_imma", Encode.string v ))
            , maybeMap record.dual_minutes (\v -> ( "dual_minutes", Encode.int v ))
            , maybeMap record.pilot_in_command_minutes (\v -> ( "pilot_in_command_minutes", Encode.int v ))
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
