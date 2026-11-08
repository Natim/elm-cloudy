module Persisted exposing (LoadedState, decoder, encodeAircraft, encodeLogbooks)

import Cloudy.Data.Aircraft as Aircraft exposing (Aircraft)
import Cloudy.Data.Logbook as Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory as LogbookCategory
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LocalLog
import LogSync exposing (Cursor)
import Outbox
import Session
import StorageCodec


type alias LoadedState =
    { session : Maybe Session.Session
    , logbooks : List Logbook
    , logs : Dict.Dict String (List LocalLog.LocalLog)
    , outbox : List Outbox.OutboxOp
    , online : Bool
    , logSync : Dict.Dict String Cursor
    , aircraft : Dict.Dict String (List Aircraft)
    }


decoder : Decoder LoadedState
decoder =
    Decode.succeed LoadedState
        |> fieldOr "session" (Decode.nullable Session.decoder) Nothing
        |> fieldOr "logbooks" (Decode.list Logbook.decoder) []
        |> fieldOr "logs" StorageCodec.decodeLogs Dict.empty
        |> fieldOr "outbox" StorageCodec.decodeOutbox []
        |> fieldOr "online" Decode.bool True
        |> fieldOr "logSync" (Decode.dict cursorDecoder) Dict.empty
        |> fieldOr "aircraft" (Decode.dict (Decode.list Aircraft.decoder)) Dict.empty


cursorDecoder : Decoder Cursor
cursorDecoder =
    Decode.map2 Cursor
        (Decode.field "complete" Decode.bool)
        (Decode.field "updatedAt" Decode.string)


fieldOr : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
fieldOr name itemDecoder fallback builder =
    Decode.map2 (\value next -> next value)
        (Decode.oneOf
            [ Decode.field name itemDecoder
            , Decode.succeed fallback
            ]
        )
        builder


encodeAircraft : Dict.Dict String (List Aircraft) -> Value
encodeAircraft aircraft =
    aircraft
        |> Dict.toList
        |> List.map (\( name, items ) -> ( name, Encode.list encodeAircraftItem items ))
        |> Encode.object


encodeAircraftItem : Aircraft -> Value
encodeAircraftItem aircraft =
    Encode.object
        [ ( "id", Encode.int aircraft.id )
        , ( "registration", Encode.string aircraft.registration )
        , ( "aircraft_type", maybeString aircraft.aircraftType )
        , ( "airport", maybeString aircraft.airport )
        , ( "photo_url", maybeString aircraft.photoUrl )
        ]


maybeString : Maybe String -> Value
maybeString text =
    case text of
        Just value ->
            Encode.string value

        Nothing ->
            Encode.null


encodeLogbooks : List Logbook -> Value
encodeLogbooks books =
    Encode.list encodeLogbook books


encodeLogbook : Logbook -> Value
encodeLogbook book =
    Encode.object
        [ ( "id", Encode.int book.id )
        , ( "name", Encode.string book.name )
        , ( "category", Encode.string (LogbookCategory.toString book.category) )
        , ( "time_format", Encode.int book.timeFormat )
        , ( "height_format", Encode.int book.heightFormat )
        , ( "created_at", Encode.string book.createdAt )
        , ( "updated_at", Encode.string book.updatedAt )
        , ( "visible", Encode.bool book.visible )
        , ( "deleted", Encode.bool book.deleted )
        ]
