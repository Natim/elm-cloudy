module Update.Outbox exposing (Msg(..), UpdateResult, update)

import Cloudy
import Cloudy.Data.Log exposing (LogEntry)
import Cloudy.Request as Request
import Dict
import Effect exposing (Effect(..))
import Http
import LogHelpers
import Model exposing (EditorState, Model)
import Outbox
import Ports
import Session
import StorageCodec
import Update.Editor
import Update.Support as Support


type Msg
    = Flush
    | GotCreate String (Result Http.Error LogEntry)
    | GotReplace (Result Http.Error LogEntry)
    | GotDelete (Result Http.Error ())


type alias UpdateResult =
    { model : Model
    , outboxCmd : Cmd Msg
    , editorCmd : Cmd Update.Editor.Msg
    , effects : List Effect
    }


update : Msg -> Model -> UpdateResult
update msg model =
    case msg of
        Flush ->
            case ( model.session, model.online, Outbox.head model.outbox ) of
                ( Just session, True, Just op ) ->
                    { model = { model | busy = True }
                    , outboxCmd = outboxCmd session op
                    , editorCmd = Cmd.none
                    , effects = []
                    }

                _ ->
                    { model = { model | busy = False }
                    , outboxCmd = Cmd.none
                    , editorCmd = Cmd.none
                    , effects = []
                    }

        GotCreate clientId result ->
            case result of
                Ok entry ->
                    let
                        serverId =
                            LogHelpers.serverId entry

                        logs =
                            Dict.map (\_ -> Outbox.applyCreateSuccess clientId serverId entry) model.logs

                        outbox =
                            Outbox.dropHead model.outbox

                        base =
                            { model | logs = logs, outbox = outbox, busy = False }

                        ( afterModel, editorCmd, editorEffects ) =
                            Update.Editor.afterOutboxCreate clientId serverId entry base
                    in
                    { model = afterModel
                    , outboxCmd =
                        Cmd.batch
                            [ Support.persistLogs logs
                            , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                            ]
                    , editorCmd = editorCmd
                    , effects = FlushOutbox :: editorEffects
                    }

                Err _ ->
                    { model = { model | busy = False, error = Just "Synchronisation interrompue.", editor = clearPendingFile model.editor }
                    , outboxCmd = Cmd.none
                    , editorCmd = Cmd.none
                    , effects = []
                    }

        GotReplace result ->
            case result of
                Ok _ ->
                    let
                        outbox =
                            Outbox.dropHead model.outbox
                    in
                    { model = { model | outbox = outbox, busy = False }
                    , outboxCmd = Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                    , editorCmd = Cmd.none
                    , effects = [ FlushOutbox ]
                    }

                Err _ ->
                    { model = { model | busy = False, error = Just "Échec de la mise à jour." }
                    , outboxCmd = Cmd.none
                    , editorCmd = Cmd.none
                    , effects = []
                    }

        GotDelete result ->
            case result of
                Ok _ ->
                    let
                        outbox =
                            Outbox.dropHead model.outbox
                    in
                    { model = { model | outbox = outbox, busy = False }
                    , outboxCmd = Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                    , editorCmd = Cmd.none
                    , effects = [ FlushOutbox ]
                    }

                Err _ ->
                    { model = { model | busy = False }
                    , outboxCmd = Cmd.none
                    , editorCmd = Cmd.none
                    , effects = []
                    }


outboxCmd : Session.Session -> Outbox.OutboxOp -> Cmd Msg
outboxCmd session op =
    let
        client =
            Session.toClient session
    in
    case op of
        Outbox.CreateOp payload ->
            Cloudy.createLog client payload.logbookId payload.entry
                |> Request.send (GotCreate payload.clientId)

        Outbox.ReplaceOp payload ->
            Cloudy.replaceLog client payload.category payload.logbookId payload.serverId payload.entry
                |> Request.send GotReplace

        Outbox.DeleteOp payload ->
            Cloudy.deleteLog client payload.logbookId payload.serverId
                |> Request.sendEmpty GotDelete


clearPendingFile : Maybe EditorState -> Maybe EditorState
clearPendingFile editor =
    Maybe.map (\current -> { current | pendingFile = Nothing, uploadingFile = False }) editor
