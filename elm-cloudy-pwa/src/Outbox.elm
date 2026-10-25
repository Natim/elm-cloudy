module Outbox exposing
    ( OutboxOp(..)
    , applyCreateSuccess
    , applyReplaceSuccess
    , decodeOp
    , dropHead
    , encodeOp
    , enqueueCreate
    , enqueueReplace
    , head
    , markHeadFailed
    , mergeServerPage
    )

import Cloudy.Data.Log as Log exposing (LogEntry)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import Set


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
    let
        serverKey =
            "s-" ++ String.fromInt serverId
    in
    List.filterMap
        (\log ->
            if log.clientId == clientId then
                Just { log | serverId = Just serverId, syncStatus = Synced, entry = entry }

            else if log.serverId == Just serverId || log.clientId == serverKey then
                Nothing

            else
                Just log
        )
        logs


applyReplaceSuccess : Int -> LogEntry -> List LocalLog -> List LocalLog
applyReplaceSuccess serverId entry logs =
    let
        savedId =
            LogHelpers.serverId entry

        same log =
            log.serverId == Just serverId || log.serverId == Just savedId
    in
    case List.filter same logs of
        [] ->
            logs

        kept :: _ ->
            { kept | serverId = Just savedId, syncStatus = Synced, entry = entry }
                :: List.filter (\log -> not (same log)) logs


mergeServerPage : Bool -> List LocalLog -> List LocalLog -> List LocalLog
mergeServerPage replace incoming existing =
    let
        pending =
            List.filter (\log -> log.syncStatus /= Synced) existing

        previous =
            List.filter (\log -> log.syncStatus == Synced) existing

        base =
            if replace then
                pending

            else
                pending ++ previous
    in
    base ++ dropKnown (serverIds base) incoming


serverIds : List LocalLog -> Set.Set Int
serverIds logs =
    List.foldl
        (\log ids ->
            case log.serverId of
                Just id ->
                    Set.insert id ids

                Nothing ->
                    ids
        )
        Set.empty
        logs


dropKnown : Set.Set Int -> List LocalLog -> List LocalLog
dropKnown ids logs =
    List.filter
        (\log ->
            case log.serverId of
                Just id ->
                    not (Set.member id ids)

                Nothing ->
                    True
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
