module Persisted exposing (LoadedState, decoder, encodeLogbooks)

import Cloudy.Data.Logbook as Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory as LogbookCategory
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LocalLog
import Outbox
import Session
import StorageCodec


type alias LoadedState =
    { session : Maybe Session.Session
    , logbooks : List Logbook
    , logs : Dict.Dict String (List LocalLog.LocalLog)
    , outbox : List Outbox.OutboxOp
    , online : Bool
    }


decoder : Decoder LoadedState
decoder =
    Decode.map5 LoadedState
        (Decode.maybe (Decode.field "session" Session.decoder))
        (Decode.field "logbooks" (Decode.list Logbook.decoder))
        (Decode.field "logs" StorageCodec.decodeLogs)
        (Decode.field "outbox" StorageCodec.decodeOutbox)
        (Decode.field "online" Decode.bool)


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
