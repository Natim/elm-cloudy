module Outbox exposing
    ( OutboxOp(..)
    , applyCreateSuccess
    , decodeOp
    , dropHead
    , encodeOp
    , enqueueCreate
    , enqueueReplace
    , head
    , markHeadFailed
    )

import Cloudy.Data.Log as Log exposing (LogEntry)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers


type OutboxOp
    = CreateOp { clientId : String, logbookId : Int, category : LogbookCategory, entry : LogEntry }
    | ReplaceOp { logbookId : Int, category : LogbookCategory, serverId : Int, entry : LogEntry }
    | DeleteOp { logbookId : Int, serverId : Int }


head : List OutboxOp -> Maybe OutboxOp
head ops =
    List.head ops


dropHead : List OutboxOp -> List OutboxOp
dropHead ops =
    case ops of
        [] ->
            []

        _ :: tail ->
            tail


markHeadFailed : String -> List OutboxOp -> List OutboxOp
markHeadFailed _ ops =
    case ops of
        [] ->
            []

        _ :: tail ->
            tail


enqueueCreate : String -> LogbookCategory -> LogEntry -> List OutboxOp -> List OutboxOp
enqueueCreate clientId category entry ops =
    CreateOp { clientId = clientId, logbookId = LogHelpers.logbookId entry, category = category, entry = entry } :: ops


enqueueReplace : LogbookCategory -> Int -> LogEntry -> List OutboxOp -> List OutboxOp
enqueueReplace category serverId entry ops =
    ReplaceOp { logbookId = LogHelpers.logbookId entry, category = category, serverId = serverId, entry = entry } :: ops


applyCreateSuccess : String -> Int -> LogEntry -> List LocalLog -> List LocalLog
applyCreateSuccess clientId serverId entry logs =
    List.map
        (\log ->
            if log.clientId == clientId then
                { log | serverId = Just serverId, syncStatus = Synced, entry = entry }

            else
                log
        )
        logs


type alias CreatePartial =
    { clientId : String
    , logbookId : Int
    , category : LogbookCategory
    , entryJson : Decode.Value
    }


type alias ReplacePartial =
    { logbookId : Int
    , category : LogbookCategory
    , serverId : Int
    , entryJson : Decode.Value
    }


decodeCreatePartial : CreatePartial -> Decoder OutboxOp
decodeCreatePartial partial =
    case Decode.decodeValue (Log.decoder partial.category) partial.entryJson of
        Ok entry ->
            Decode.succeed (CreateOp { clientId = partial.clientId, logbookId = partial.logbookId, category = partial.category, entry = entry })

        Err err ->
            Decode.fail (Decode.errorToString err)


decodeReplacePartial : ReplacePartial -> Decoder OutboxOp
decodeReplacePartial partial =
    case Decode.decodeValue (Log.decoder partial.category) partial.entryJson of
        Ok entry ->
            Decode.succeed (ReplaceOp { logbookId = partial.logbookId, category = partial.category, serverId = partial.serverId, entry = entry })

        Err err ->
            Decode.fail (Decode.errorToString err)


encodeOp : OutboxOp -> Value
encodeOp op =
    case op of
        CreateOp payload ->
            Encode.object
                [ ( "kind", Encode.string "create" )
                , ( "clientId", Encode.string payload.clientId )
                , ( "logbookId", Encode.int payload.logbookId )
                , ( "category", Encode.string (LogbookCategory.toString payload.category) )
                , ( "entry", Log.encodeReplace payload.entry )
                ]

        ReplaceOp payload ->
            Encode.object
                [ ( "kind", Encode.string "replace" )
                , ( "logbookId", Encode.int payload.logbookId )
                , ( "category", Encode.string (LogbookCategory.toString payload.category) )
                , ( "serverId", Encode.int payload.serverId )
                , ( "entry", Log.encodeReplace payload.entry )
                ]

        DeleteOp payload ->
            Encode.object
                [ ( "kind", Encode.string "delete" )
                , ( "logbookId", Encode.int payload.logbookId )
                , ( "serverId", Encode.int payload.serverId )
                ]


decodeOp : Decoder OutboxOp
decodeOp =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "create" ->
                        Decode.map4 CreatePartial
                            (Decode.field "clientId" Decode.string)
                            (Decode.field "logbookId" Decode.int)
                            (Decode.field "category" LogbookCategory.decoder)
                            (Decode.field "entry" Decode.value)
                            |> Decode.andThen decodeCreatePartial

                    "replace" ->
                        Decode.map4 ReplacePartial
                            (Decode.field "logbookId" Decode.int)
                            (Decode.field "category" LogbookCategory.decoder)
                            (Decode.field "serverId" Decode.int)
                            (Decode.field "entry" Decode.value)
                            |> Decode.andThen decodeReplacePartial

                    "delete" ->
                        Decode.map2
                            (\logbookId serverId ->
                                DeleteOp { logbookId = logbookId, serverId = serverId }
                            )
                            (Decode.field "logbookId" Decode.int)
                            (Decode.field "serverId" Decode.int)

                    _ ->
                        Decode.fail ("Unknown outbox kind: " ++ kind)
            )
