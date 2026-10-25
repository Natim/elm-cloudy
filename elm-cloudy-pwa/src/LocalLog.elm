module LocalLog exposing
    ( LocalLog
    , SyncStatus(..)
    , categoryOf
    , clientIdOf
    , decode
    , encode
    , logbookIdOf
    , pendingCount
    , serverIdOf
    , statusLabel
    )

import Cloudy.Data.Log as Log exposing (LogEntry)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import LogHelpers


type SyncStatus
    = Synced
    | PendingCreate
    | PendingUpdate
    | PendingDelete
    | Failed String


type alias LocalLog =
    { clientId : String
    , serverId : Maybe Int
    , syncStatus : SyncStatus
    , category : LogbookCategory
    , entry : LogEntry
    }


categoryOf : LocalLog -> LogbookCategory
categoryOf log =
    log.category


logbookIdOf : LocalLog -> Int
logbookIdOf log =
    LogHelpers.logbookId log.entry


serverIdOf : LocalLog -> Maybe Int
serverIdOf log =
    log.serverId


clientIdOf : LocalLog -> String
clientIdOf log =
    log.clientId


statusLabel : SyncStatus -> String
statusLabel status =
    case status of
        Synced ->
            "à jour"

        PendingCreate ->
            "en attente"

        PendingUpdate ->
            "modif en attente"

        PendingDelete ->
            "suppr. en attente"

        Failed _ ->
            "échec"


pendingCount : List LocalLog -> Int
pendingCount logs =
    List.length (List.filter (\log -> log.syncStatus /= Synced) logs)


syncStatusEncoder : SyncStatus -> Value
syncStatusEncoder status =
    case status of
        Synced ->
            Encode.string "synced"

        PendingCreate ->
            Encode.string "pending_create"

        PendingUpdate ->
            Encode.string "pending_update"

        PendingDelete ->
            Encode.string "pending_delete"

        Failed reason ->
            Encode.list Encode.string [ "failed", reason ]


syncStatusDecoder : Decoder SyncStatus
syncStatusDecoder =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\raw ->
                    case raw of
                        "synced" ->
                            Decode.succeed Synced

                        "pending_create" ->
                            Decode.succeed PendingCreate

                        "pending_update" ->
                            Decode.succeed PendingUpdate

                        "pending_delete" ->
                            Decode.succeed PendingDelete

                        _ ->
                            Decode.fail ("Unknown sync status: " ++ raw)
                )
        , Decode.list Decode.string
            |> Decode.andThen
                (\items ->
                    case items of
                        "failed" :: reason :: _ ->
                            Decode.succeed (Failed reason)

                        _ ->
                            Decode.fail "Invalid failed status"
                )
        ]


encode : LocalLog -> Value
encode log =
    Encode.object
        [ ( "clientId", Encode.string log.clientId )
        , ( "serverId", encodeMaybeInt log.serverId )
        , ( "syncStatus", syncStatusEncoder log.syncStatus )
        , ( "category", Encode.string (LogbookCategory.toString log.category) )
        , ( "entry", Log.encodeReplace log.entry )
        ]


encodeMaybeInt : Maybe Int -> Value
encodeMaybeInt maybeInt =
    case maybeInt of
        Just n ->
            Encode.int n

        Nothing ->
            Encode.null


type alias PartialLocalLog =
    { clientId : String
    , serverId : Maybe Int
    , syncStatus : SyncStatus
    , category : LogbookCategory
    , entryJson : Decode.Value
    }


partialDecoder : Decoder PartialLocalLog
partialDecoder =
    Decode.map5 PartialLocalLog
        (Decode.field "clientId" Decode.string)
        (Decode.field "serverId" (Decode.nullable Decode.int))
        (Decode.field "syncStatus" syncStatusDecoder)
        (Decode.field "category" LogbookCategory.decoder)
        (Decode.field "entry" Decode.value)


decode : Decoder LocalLog
decode =
    partialDecoder
        |> Decode.andThen
            (\partial ->
                decodeFromFields partial.clientId partial.serverId partial.syncStatus partial.category partial.entryJson
            )


decodeFromFields : String -> Maybe Int -> SyncStatus -> LogbookCategory -> Decode.Value -> Decoder LocalLog
decodeFromFields clientId serverId syncStatus category entryJson =
    case Decode.decodeValue (Log.decoder category) entryJson of
        Ok entry ->
            Decode.succeed
                { clientId = clientId
                , serverId = serverId
                , syncStatus = syncStatus
                , category = category
                , entry = entry
                }

        Err err ->
            Decode.fail (Decode.errorToString err)
