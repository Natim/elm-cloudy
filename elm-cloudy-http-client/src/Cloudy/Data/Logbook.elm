module Cloudy.Data.Logbook exposing (Logbook, LogbookCreate, LogbookPatch, decoder, encodeCreate, encodePatch)

import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Logbook =
    { id : Int
    , name : String
    , category : LogbookCategory
    , timeFormat : Int
    , heightFormat : Int
    , createdAt : String
    , updatedAt : String
    , visible : Bool
    , deleted : Bool
    }


type alias LogbookCreate =
    { category : LogbookCategory
    , name : Maybe String
    }


type alias LogbookPatch =
    { name : Maybe String
    , timeFormat : Maybe Int
    , heightFormat : Maybe Int
    }


decoder : Decoder Logbook
decoder =
    Decode.succeed Logbook
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "category" LogbookCategory.decoder
        |> required "time_format" Decode.int
        |> required "height_format" Decode.int
        |> required "created_at" Decode.string
        |> required "updated_at" Decode.string
        |> required "visible" Decode.bool
        |> required "deleted" Decode.bool


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field dec builder =
    Decode.map2 (\v f -> f v) (Decode.field field dec) builder


encodeCreate : LogbookCreate -> Value
encodeCreate payload =
    Encode.object
        (List.filterMap identity
            [ Just ( "category", Encode.string (LogbookCategory.toString payload.category) )
            , Maybe.map (\name -> ( "name", Encode.string name )) payload.name
            ]
        )


encodePatch : LogbookPatch -> Value
encodePatch payload =
    Encode.object
        (List.filterMap identity
            [ Maybe.map (\name -> ( "name", Encode.string name )) payload.name
            , Maybe.map (\v -> ( "time_format", Encode.int v )) payload.timeFormat
            , Maybe.map (\v -> ( "height_format", Encode.int v )) payload.heightFormat
            ]
        )
