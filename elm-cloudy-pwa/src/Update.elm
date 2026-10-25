module Update exposing (update)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cloudy
import Cloudy.Client as Client
import Cloudy.Data.LogListPage exposing (LogListPage)
import Cloudy.Data.Logbook as Logbook
import Cloudy.Data.LogbookCategory as LogbookCategory
import Cloudy.Request as Request
import Dict
import Http
import Json.Decode
import Json.Encode as Encode
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import Model exposing (EditorState, Model, Msg(..))
import Outbox exposing (OutboxOp(..))
import Persisted
import Ports
import Route exposing (Route(..))
import Session
import StorageCodec
import Task
import TimeFormat
import Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url

                editor =
                    case ( route, model.editor ) of
                        ( EditorEdit id clientId, Just current ) ->
                            if current.logbookId == id && current.localLog.clientId == clientId then
                                Just current

                            else
                                Nothing

                        ( EditorNew id, Just current ) ->
                            if current.logbookId == id then
                                Just current

                            else
                                Nothing

                        _ ->
                            Nothing

                cmd =
                    case route of
                        Logs id ->
                            cmdMsg (RefreshLogs id)

                        EditorEdit id clientId ->
                            case editor of
                                Just _ ->
                                    Cmd.none

                                Nothing ->
                                    cmdMsg (EditLog id clientId)

                        EditorNew id ->
                            if editor == Nothing then
                                cmdMsg (StartNewLog id)

                            else
                                Cmd.none

                        Login ->
                            if model.session /= Nothing then
                                Nav.pushUrl model.key (Route.toPath Logbooks)

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | route = route, editor = editor }, cmd )

        StorageLoaded raw ->
            case Json.Decode.decodeValue Persisted.decoder raw of
                Ok loaded ->
                    ( { model
                        | session = loaded.session
                        , logbooks = loaded.logbooks
                        , logs = loaded.logs
                        , outbox = loaded.outbox
                        , online = loaded.online
                        , storageReady = True
                        , route =
                            if loaded.session == Nothing then
                                Login

                            else if model.route == Login then
                                Logbooks

                            else
                                model.route
                      }
                    , if loaded.session /= Nothing then
                        Cmd.batch
                            [ cmdMsg RefreshLogbooks
                            , cmdMsg FlushOutbox
                            , case model.route of
                                Login ->
                                    Nav.pushUrl model.key (Route.toPath Logbooks)

                                Logs id ->
                                    cmdMsg (RefreshLogs id)

                                EditorEdit id clientId ->
                                    cmdMsg (EditLog id clientId)

                                EditorNew id ->
                                    if model.editor == Nothing then
                                        cmdMsg (StartNewLog id)

                                    else
                                        Cmd.none

                                _ ->
                                    Cmd.none
                            ]

                      else
                        Cmd.none
                    )

                Err _ ->
                    ( { model | storageReady = True }, Cmd.none )

        ConnectivityChanged online ->
            ( { model | online = online }
            , if online then
                cmdMsg FlushOutbox

              else
                Cmd.none
            )

        LoginEmailChanged v ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | email = v } }, Cmd.none )

        LoginPasswordChanged v ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | password = v } }, Cmd.none )

        LoginBaseUrlChanged v ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | baseUrl = v } }, Cmd.none )

        LoginAdvancedToggled open ->
            let
                form =
                    model.loginForm
            in
            ( { model | loginForm = { form | advancedOpen = open } }, Cmd.none )

        LoginSubmitted ->
            let
                client =
                    { baseUrl = model.loginForm.baseUrl, auth = Client.NoAuth }
            in
            ( { model | busy = True, error = Nothing }
            , Cloudy.token client { email = model.loginForm.email, password = model.loginForm.password }
                |> Request.send GotToken
            )

        GotToken result ->
            case result of
                Ok token ->
                    let
                        session =
                            { baseUrl = model.loginForm.baseUrl
                            , accessToken = token.accessToken
                            , userId = token.userId
                            }
                    in
                    ( { model | session = Just session, busy = False, error = Nothing, route = Logbooks }
                    , Cmd.batch
                        [ Ports.storageSaveSession (Session.encode session)
                        , Nav.pushUrl model.key (Route.toPath Logbooks)
                        , cmdMsg RefreshLogbooks
                        , cmdMsg FlushOutbox
                        ]
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Identifiants invalides." }, Cmd.none )

        RefreshLogbooks ->
            case model.session of
                Nothing ->
                    ( model, Cmd.none )

                Just session ->
                    ( { model | busy = True }
                    , Cloudy.listLogbooks (Session.toClient session)
                        |> Request.send GotLogbooks
                    )

        GotLogbooks result ->
            case result of
                Ok books ->
                    ( { model | logbooks = books, busy = False }
                    , Ports.storageSaveLogbooks (Persisted.encodeLogbooks books)
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Impossible de charger les carnets." }, Cmd.none )

        SelectLogbook id ->
            ( model, Nav.pushUrl model.key (Route.toPath (Logs id)) )

        BackToLogbooks ->
            ( model, Nav.pushUrl model.key (Route.toPath Logbooks) )

        RefreshLogs logbookId ->
            case ( model.session, findLogbook model logbookId ) of
                ( Just session, Just book ) ->
                    let
                        client =
                            Session.toClient session
                    in
                    ( { model | busy = True }
                    , Cmd.batch
                        [ Cloudy.listLogs client book.category logbookId { limit = Just 100, pageToken = Nothing, dateFrom = Nothing, dateTo = Nothing }
                            |> Request.send (GotLogsPage logbookId book.category)
                        , Cloudy.getReports client book.category logbookId
                            |> Request.send (GotReports logbookId)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotLogsPage logbookId category result ->
            case result of
                Ok page ->
                    let
                        merged =
                            mergePage category page

                        key =
                            String.fromInt logbookId

                        pending =
                            Dict.get key model.logs
                                |> Maybe.withDefault []
                                |> List.filter (\log -> log.syncStatus /= Synced)

                        logs =
                            Dict.insert key (pending ++ merged) model.logs
                    in
                    ( { model | logs = logs, busy = False, error = Nothing }
                    , persistLogs logs
                    )

                Err err ->
                    ( { model | busy = False, error = Just (httpErrorMessage "vols" err) }, Cmd.none )

        GotReports logbookId result ->
            case result of
                Ok report ->
                    ( { model | reports = Dict.insert (String.fromInt logbookId) report model.reports }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        StartNewLog logbookId ->
            case findLogbook model logbookId of
                Nothing ->
                    ( model, Cmd.none )

                Just book ->
                    ( { model | pendingNew = Just ( logbookId, book.category ) }
                    , Ports.nowRequest ()
                    )

        NowReceived iso ->
            case model.pendingNew of
                Nothing ->
                    ( model, Cmd.none )

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
                    ( { model | editor = Just { logbookId = logbookId, category = category, localLog = localLog, dateDraft = Nothing }, pendingNew = Nothing }
                    , if model.route == EditorNew logbookId then
                        Cmd.none

                      else
                        Nav.pushUrl model.key (Route.toPath (EditorNew logbookId))
                    )

        EditLog logbookId clientId ->
            let
                missing =
                    if model.storageReady && model.route == EditorEdit logbookId clientId then
                        ( model, Nav.pushUrl model.key (Route.toPath (Logs logbookId)) )

                    else
                        ( model, Cmd.none )

                open localLog =
                    ( { model | editor = Just { logbookId = logbookId, category = localLog.category, localLog = localLog, dateDraft = Nothing } }
                    , if model.route == EditorEdit logbookId clientId then
                        Cmd.none

                      else
                        Nav.pushUrl model.key (Route.toPath (EditorEdit logbookId clientId))
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

        EditorChanged entry ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none )

                Just editor ->
                    let
                        localLog =
                            editor.localLog
                    in
                    ( { model | editor = Just { editor | localLog = { localLog | entry = entry } } }, Cmd.none )

        DateTextChanged raw ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none )

                Just editor ->
                    let
                        updated =
                            case TimeFormat.parseDdMmYyyy raw of
                                Just isoDay ->
                                    setEditorDate isoDay { editor | dateDraft = Just raw }

                                Nothing ->
                                    { editor | dateDraft = Just raw }
                    in
                    ( { model | editor = Just updated }, Cmd.none )

        DatePicked isoDay ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none )

                Just editor ->
                    ( { model | editor = Just (setEditorDate isoDay { editor | dateDraft = Nothing }) }, Cmd.none )

        DateTextBlurred ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none )

                Just editor ->
                    ( { model | editor = Just { editor | dateDraft = Nothing } }, Cmd.none )

        SaveEditor ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none )

                Just editor ->
                    let
                        logbookKey =
                            String.fromInt editor.logbookId

                        existing =
                            Dict.get logbookKey model.logs |> Maybe.withDefault []

                        ( localLog, outbox ) =
                            saveLocalLog editor.localLog existing model.outbox

                        logs =
                            Dict.insert logbookKey (upsertLocal existing localLog) model.logs
                    in
                    ( { model | logs = logs, outbox = outbox }
                    , Cmd.batch
                        [ persistLogs logs
                        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                        , Nav.pushUrl model.key (Route.toPath (Logs editor.logbookId))
                        , cmdMsg FlushOutbox
                        ]
                    )

        CancelEditor ->
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
            ( model
            , if logbookId == 0 then
                Nav.pushUrl model.key (Route.toPath Logbooks)

              else
                Nav.pushUrl model.key (Route.toPath (Logs logbookId))
            )

        FlushOutbox ->
            case ( model.session, model.online, Outbox.head model.outbox ) of
                ( Just session, True, Just op ) ->
                    ( { model | busy = True }, outboxCmd session op )

                _ ->
                    ( { model | busy = False }, Cmd.none )

        GotOutboxCreate clientId result ->
            case result of
                Ok entry ->
                    let
                        serverId =
                            LogHelpers.serverId entry

                        logs =
                            Dict.map (\_ -> Outbox.applyCreateSuccess clientId serverId entry) model.logs

                        outbox =
                            Outbox.dropHead model.outbox
                    in
                    ( { model | logs = logs, outbox = outbox, busy = False }
                    , Cmd.batch
                        [ persistLogs logs
                        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                        , cmdMsg FlushOutbox
                        ]
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Synchronisation interrompue." }, Cmd.none )

        GotOutboxReplace result ->
            case result of
                Ok _ ->
                    let
                        outbox =
                            Outbox.dropHead model.outbox
                    in
                    ( { model | outbox = outbox, busy = False }
                    , Cmd.batch
                        [ Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                        , cmdMsg FlushOutbox
                        ]
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Échec de la mise à jour." }, Cmd.none )

        GotOutboxDelete result ->
            case result of
                Ok _ ->
                    let
                        outbox =
                            Outbox.dropHead model.outbox
                    in
                    ( { model | outbox = outbox, busy = False }
                    , Cmd.batch
                        [ Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                        , cmdMsg FlushOutbox
                        ]
                    )

                Err _ ->
                    ( { model | busy = False }, Cmd.none )

        Logout ->
            ( { model | session = Nothing, route = Login, logbooks = [], logs = Dict.empty, reports = Dict.empty, outbox = [] }
            , Cmd.batch
                [ Ports.storageSaveSession Encode.null
                , Ports.storageSaveLogbooks (Persisted.encodeLogbooks [])
                , Ports.storageSaveLogs (StorageCodec.encodeLogs Dict.empty)
                , Ports.storageSaveOutbox (StorageCodec.encodeOutbox [])
                , Nav.pushUrl model.key (Route.toPath Login)
                ]
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


findLogbook : Model -> Int -> Maybe Logbook.Logbook
findLogbook model id =
    List.filter (\b -> b.id == id) model.logbooks |> List.head


mergePage : LogbookCategory.LogbookCategory -> LogListPage -> List LocalLog
mergePage category page =
    List.map
        (\entry ->
            { clientId = "s-" ++ String.fromInt (LogHelpers.serverId entry)
            , serverId = Just (LogHelpers.serverId entry)
            , syncStatus = Synced
            , category = category
            , entry = entry
            }
        )
        page.items


saveLocalLog : LocalLog -> List LocalLog -> List Outbox.OutboxOp -> ( LocalLog, List Outbox.OutboxOp )
saveLocalLog localLog _ outbox =
    case localLog.serverId of
        Nothing ->
            ( { localLog | syncStatus = PendingCreate }
            , Outbox.enqueueCreate localLog.clientId localLog.category localLog.entry (removeCreate localLog.clientId outbox)
            )

        Just serverId ->
            ( { localLog | syncStatus = PendingUpdate }
            , Outbox.enqueueReplace localLog.category serverId localLog.entry outbox
            )


removeCreate : String -> List Outbox.OutboxOp -> List Outbox.OutboxOp
removeCreate clientId ops =
    List.filter
        (\op ->
            case op of
                CreateOp payload ->
                    payload.clientId /= clientId

                _ ->
                    True
        )
        ops


upsertLocal : List LocalLog -> LocalLog -> List LocalLog
upsertLocal logs localLog =
    localLog :: List.filter (\l -> l.clientId /= localLog.clientId) logs


persistLogs : Dict.Dict String (List LocalLog) -> Cmd Msg
persistLogs logs =
    Ports.storageSaveLogs (StorageCodec.encodeLogs logs)


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


cmdMsg : Msg -> Cmd Msg
cmdMsg message =
    Task.succeed message |> Task.perform identity


outboxCmd : Session.Session -> Outbox.OutboxOp -> Cmd Msg
outboxCmd session op =
    let
        client =
            Session.toClient session
    in
    case op of
        CreateOp payload ->
            Cloudy.createLog client payload.logbookId payload.entry
                |> Request.send (GotOutboxCreate payload.clientId)

        ReplaceOp payload ->
            Cloudy.replaceLog client payload.category payload.logbookId payload.serverId payload.entry
                |> Request.send GotOutboxReplace

        DeleteOp payload ->
            Cloudy.deleteLog client payload.logbookId payload.serverId
                |> Request.send GotOutboxDelete
