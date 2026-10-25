module Update.Support exposing
    ( appendLogPage
    , chosenLogbook
    , findLogbook
    , httpErrorMessage
    , logout
    , persistLogs
    , removeCreate
    , saveLocalLog
    , unauthorized
    , upsertLocal
    )

import Browser.Navigation as Nav
import Cloudy.Data.Logbook as Logbook
import Dict
import Http
import Json.Encode as Encode
import LocalLog exposing (LocalLog)
import Model exposing (Model)
import Outbox
import Persisted
import Ports
import Route exposing (Route(..))
import StorageCodec


findLogbook : Model -> Int -> Maybe Logbook.Logbook
findLogbook model id =
    List.filter (\b -> b.id == id) model.logbooks |> List.head


chosenLogbook : Model -> Route -> Maybe Int
chosenLogbook model route =
    case Route.logbookId route of
        Just id ->
            Just id

        Nothing ->
            model.selectedLogbookId


persistLogs : Dict.Dict String (List LocalLog) -> Cmd msg
persistLogs logs =
    Ports.storageSaveLogs (StorageCodec.encodeLogs logs)


logout : Model -> ( Model, Cmd msg )
logout model =
    ( { model
        | session = Nothing
        , route = Login
        , logbooks = []
        , logs = Dict.empty
        , reports = Dict.empty
        , outbox = []
        , editor = Nothing
        , navOpen = False
        , selectedLogbookId = Nothing
        , aircraft = Dict.empty
        , aircraftEditor = Nothing
        , pendingAircraft = Nothing
        , logFetchId = 0
        , logPaging = Nothing
        , logsPartial = Nothing
      }
    , Cmd.batch
        [ Ports.storageSaveSession Encode.null
        , Ports.storageSaveLogbooks (Persisted.encodeLogbooks [])
        , Ports.storageSaveLogs (StorageCodec.encodeLogs Dict.empty)
        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox [])
        , Nav.pushUrl model.key (Route.toPath Login)
        ]
    )


unauthorized : Http.Error -> Bool
unauthorized err =
    case err of
        Http.BadStatus 401 ->
            True

        _ ->
            False


httpErrorMessage : String -> Http.Error -> String
httpErrorMessage what err =
    let
        detail =
            case err of
                Http.BadUrl url ->
                    "URL invalide : " ++ url

                Http.Timeout ->
                    "Délai dépassé."

                Http.NetworkError ->
                    "Erreur réseau."

                Http.BadStatus code ->
                    "Réponse HTTP " ++ String.fromInt code ++ "."

                Http.BadBody message ->
                    message
    in
    "Impossible de lire les " ++ what ++ ".\n" ++ detail


saveLocalLog : LocalLog -> List LocalLog -> List Outbox.OutboxOp -> ( LocalLog, List Outbox.OutboxOp )
saveLocalLog localLog _ outbox =
    case localLog.serverId of
        Nothing ->
            ( { localLog | syncStatus = LocalLog.PendingCreate }
            , Outbox.enqueueCreate localLog.clientId localLog.category localLog.entry (removeCreate localLog.clientId outbox)
            )

        Just serverId ->
            ( { localLog | syncStatus = LocalLog.PendingUpdate }
            , Outbox.enqueueReplace localLog.category serverId localLog.entry outbox
            )


removeCreate : String -> List Outbox.OutboxOp -> List Outbox.OutboxOp
removeCreate clientId ops =
    List.filter
        (\op ->
            case op of
                Outbox.CreateOp payload ->
                    payload.clientId /= clientId

                _ ->
                    True
        )
        ops


upsertLocal : List LocalLog -> LocalLog -> List LocalLog
upsertLocal logs localLog =
    localLog :: List.filter (\l -> l.clientId /= localLog.clientId) logs


appendLogPage : String -> Bool -> List LocalLog -> Dict.Dict String (List LocalLog) -> List LocalLog
appendLogPage key replace incoming logs =
    let
        existing =
            Dict.get key logs |> Maybe.withDefault []
    in
    Outbox.mergeServerPage replace incoming existing
