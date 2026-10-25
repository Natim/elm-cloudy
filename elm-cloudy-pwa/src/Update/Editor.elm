module Update.Editor exposing (Msg(..), afterOutboxCreate, update)

import Browser.Navigation as Nav
import Cloudy
import Cloudy.Data.AttachedFile exposing (AttachedFile)
import Cloudy.Data.Log exposing (LogEntry)
import Cloudy.Request as Request
import Dict
import Effect exposing (Effect(..))
import File
import Http
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import Model exposing (EditorState, Model)
import Ports
import Route exposing (Route(..))
import Session
import StorageCodec
import TimeFormat
import Update.Support as Support


type Msg
    = StartNew Int
    | NowReceived String
    | Edit Int String
    | Changed LogEntry
    | DateTextChanged String
    | DatePicked String
    | DateTextBlurred
    | GotLogFile File.File
    | GotLogFileUpload (Result Http.Error AttachedFile)
    | Save
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg, List Effect )
update msg model =
    case msg of
        StartNew logbookId ->
            case Support.findLogbook model logbookId of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just book ->
                    ( { model | pendingNew = Just ( logbookId, book.category ) }
                    , Ports.nowRequest ()
                    , []
                    )

        NowReceived iso ->
            case model.pendingNew of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just ( logbookId, category ) ->
                    let
                        clientId =
                            "c-" ++ String.fromInt logbookId ++ "-" ++ String.left 16 iso

                        entry =
                            LogHelpers.emptyEntry category logbookId iso

                        localLog =
                            { clientId = clientId
                            , serverId = Nothing
                            , syncStatus = PendingCreate
                            , category = category
                            , entry = entry
                            }
                    in
                    ( { model | editor = Just { logbookId = logbookId, category = category, localLog = localLog, dateDraft = Nothing, uploadingFile = False, pendingFile = Nothing }, pendingNew = Nothing }
                    , if model.route == EditorNew logbookId then
                        Cmd.none

                      else
                        Nav.pushUrl model.key (Route.toPath (EditorNew logbookId))
                    , []
                    )

        Edit logbookId clientId ->
            let
                missing =
                    if model.storageReady && model.route == EditorEdit logbookId clientId then
                        ( model, Nav.pushUrl model.key (Route.toPath (Logs logbookId)), [] )

                    else
                        ( model, Cmd.none, [] )

                open localLog =
                    ( { model | editor = Just { logbookId = logbookId, category = localLog.category, localLog = localLog, dateDraft = Nothing, uploadingFile = False, pendingFile = Nothing } }
                    , if model.route == EditorEdit logbookId clientId then
                        Cmd.none

                      else
                        Nav.pushUrl model.key (Route.toPath (EditorEdit logbookId clientId))
                    , []
                    )
            in
            case Dict.get (String.fromInt logbookId) model.logs of
                Nothing ->
                    missing

                Just logs ->
                    case List.filter (\l -> l.clientId == clientId) logs |> List.head of
                        Nothing ->
                            missing

                        Just localLog ->
                            open localLog

        Changed entry ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just editor ->
                    let
                        localLog =
                            editor.localLog
                    in
                    ( { model | editor = Just { editor | localLog = { localLog | entry = entry } } }, Cmd.none, [] )

        DateTextChanged raw ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just editor ->
                    let
                        updated =
                            case TimeFormat.parseDdMmYyyy raw of
                                Just isoDay ->
                                    setEditorDate isoDay { editor | dateDraft = Just raw }

                                Nothing ->
                                    { editor | dateDraft = Just raw }
                    in
                    ( { model | editor = Just updated }, Cmd.none, [] )

        DatePicked isoDay ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just editor ->
                    ( { model | editor = Just (setEditorDate isoDay { editor | dateDraft = Nothing }) }, Cmd.none, [] )

        DateTextBlurred ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just editor ->
                    ( { model | editor = Just { editor | dateDraft = Nothing } }, Cmd.none, [] )

        GotLogFile file ->
            case ( model.session, model.editor ) of
                ( Just session, Just editor ) ->
                    case editor.localLog.serverId of
                        Just serverId ->
                            ( { model | editor = Just { editor | uploadingFile = True, pendingFile = Nothing }, error = Nothing }
                            , Cloudy.uploadLogFile (Session.toClient session) editor.logbookId serverId file
                                |> Request.send GotLogFileUpload
                            , []
                            )

                        Nothing ->
                            saveNewLogThenUpload file editor model

                _ ->
                    ( { model | route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    , []
                    )

        GotLogFileUpload result ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just editor ->
                    case result of
                        Ok attached ->
                            let
                                localLog =
                                    editor.localLog

                                next =
                                    { localLog | entry = LogHelpers.addFile attached localLog.entry }

                                key =
                                    String.fromInt editor.logbookId

                                existing =
                                    Dict.get key model.logs |> Maybe.withDefault []

                                logs =
                                    Dict.insert key (Support.upsertLocal existing next) model.logs
                            in
                            ( { model | editor = Just { editor | localLog = next, uploadingFile = False }, logs = logs, error = Nothing }
                            , Support.persistLogs logs
                            , []
                            )

                        Err err ->
                            if Support.unauthorized err then
                                let
                                    ( next, cmd ) =
                                        Support.logout { model | busy = False }
                                in
                                ( next, cmd, [] )

                            else
                                ( { model | editor = Just { editor | uploadingFile = False }, error = Just "Impossible d'envoyer la photo." }, Cmd.none, [] )

        Save ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none, [] )

                Just editor ->
                    let
                        logbookKey =
                            String.fromInt editor.logbookId

                        existing =
                            Dict.get logbookKey model.logs |> Maybe.withDefault []

                        ( localLog, outbox ) =
                            Support.saveLocalLog editor.localLog existing model.outbox

                        logs =
                            Dict.insert logbookKey (Support.upsertLocal existing localLog) model.logs
                    in
                    ( { model | logs = logs, outbox = outbox, editor = Nothing }
                    , Cmd.batch
                        [ Support.persistLogs logs
                        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                        , Nav.pushUrl model.key (Route.toPath (Logs editor.logbookId))
                        ]
                    , [ FlushOutbox ]
                    )

        Cancel ->
            let
                logbookId =
                    case ( model.editor, model.route ) of
                        ( Just editor, _ ) ->
                            editor.logbookId

                        ( _, EditorEdit id _ ) ->
                            id

                        ( _, EditorNew id ) ->
                            id

                        ( _, Logs id ) ->
                            id

                        _ ->
                            0
            in
            ( { model | editor = Nothing }
            , if logbookId == 0 then
                Nav.pushUrl model.key (Route.toPath Dashboard)

              else
                Nav.pushUrl model.key (Route.toPath (Logs logbookId))
            , []
            )


afterOutboxCreate : String -> Int -> LogEntry -> Model -> ( Model, Cmd Msg, List Effect )
afterOutboxCreate clientId serverId entry model =
    case model.editor of
        Just editor ->
            if editor.localLog.clientId == clientId then
                let
                    current =
                        editor.localLog

                    localLog =
                        { current | serverId = Just serverId, syncStatus = Synced, entry = entry }
                in
                case ( editor.pendingFile, model.session ) of
                    ( Just file, Just session ) ->
                        ( { model | editor = Just { editor | localLog = localLog, pendingFile = Nothing, uploadingFile = True } }
                        , Cloudy.uploadLogFile (Session.toClient session) editor.logbookId serverId file
                            |> Request.send GotLogFileUpload
                        , []
                        )

                    _ ->
                        ( { model | editor = Just { editor | localLog = localLog, pendingFile = Nothing, uploadingFile = False } }
                        , Cmd.none
                        , []
                        )

            else
                ( model, Cmd.none, [] )

        Nothing ->
            ( model, Cmd.none, [] )


saveNewLogThenUpload : File.File -> EditorState -> Model -> ( Model, Cmd Msg, List Effect )
saveNewLogThenUpload file editor model =
    let
        logbookKey =
            String.fromInt editor.logbookId

        existing =
            Dict.get logbookKey model.logs |> Maybe.withDefault []

        ( localLog, savedOutbox ) =
            Support.saveLocalLog editor.localLog existing model.outbox

        outbox =
            if model.busy then
                case savedOutbox of
                    newest :: rest ->
                        rest ++ [ newest ]

                    [] ->
                        []

            else
                savedOutbox

        logs =
            Dict.insert logbookKey (Support.upsertLocal existing localLog) model.logs

        editRoute =
            EditorEdit editor.logbookId localLog.clientId
    in
    ( { model
        | editor = Just { editor | localLog = localLog, uploadingFile = True, pendingFile = Just file }
        , logs = logs
        , outbox = outbox
        , error =
            if model.online then
                Nothing

            else
                Just "Hors ligne. La photo sera envoyée à la synchronisation."
      }
    , Cmd.batch
        [ Support.persistLogs logs
        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
        , if model.route == editRoute then
            Cmd.none

          else
            Nav.pushUrl model.key (Route.toPath editRoute)
        ]
    , if model.busy then
        []

      else
        [ FlushOutbox ]
    )


setEditorDate : String -> EditorState -> EditorState
setEditorDate isoDay editor =
    let
        localLog =
            editor.localLog

        next =
            TimeFormat.applyIsoDate isoDay (LogHelpers.flightDate localLog.entry)
    in
    { editor | localLog = { localLog | entry = LogHelpers.setFlightDate next localLog.entry } }
