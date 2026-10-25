module Update exposing (update)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cloudy
import Cloudy.Client as Client
import Cloudy.Data.Aircraft as AircraftData
import Cloudy.Data.Log exposing (LogEntry)
import Cloudy.Data.LogListPage exposing (LogListPage)
import Cloudy.Data.Logbook as Logbook
import Cloudy.Data.LogbookCategory as LogbookCategory
import Cloudy.Request as Request
import Dict
import File
import Http
import Json.Decode
import Json.Encode as Encode
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import Model exposing (AircraftEditor, EditorState, Model, Msg(..))
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

                aircraftEditor =
                    case ( route, model.aircraftEditor ) of
                        ( AircraftNew id, Just current ) ->
                            if current.logbookId == id && current.aircraftId == Nothing then
                                Just current

                            else
                                Nothing

                        ( AircraftEdit id aircraftId, Just current ) ->
                            if current.logbookId == id && current.aircraftId == Just aircraftId then
                                Just current

                            else
                                Nothing

                        _ ->
                            Nothing

                pendingAircraft =
                    case ( route, model.pendingAircraft ) of
                        ( AircraftEdit id aircraftId, Just ( pendingId, pendingAircraftId ) ) ->
                            if id == pendingId && aircraftId == pendingAircraftId then
                                Just ( pendingId, pendingAircraftId )

                            else
                                Nothing

                        _ ->
                            Nothing

                selectedLogbookId =
                    case Route.logbookId route of
                        Just id ->
                            Just id

                        Nothing ->
                            model.selectedLogbookId

                next =
                    { model
                        | route = route
                        , editor = editor
                        , aircraftEditor = aircraftEditor
                        , pendingAircraft = pendingAircraft
                        , navOpen = False
                        , selectedLogbookId = selectedLogbookId
                        , error = Nothing
                    }
            in
            ( next, enterRoute next route )

        StorageLoaded raw ->
            case Json.Decode.decodeValue Persisted.decoder raw of
                Ok loaded ->
                    let
                        route =
                            if loaded.session == Nothing then
                                Login

                            else if model.route == Login then
                                Dashboard

                            else
                                model.route

                        selectedLogbookId =
                            case Route.logbookId route of
                                Just id ->
                                    Just id

                                Nothing ->
                                    loaded.logbooks |> List.head |> Maybe.map .id

                        next =
                            { model
                                | session = loaded.session
                                , logbooks = loaded.logbooks
                                , logs = loaded.logs
                                , outbox = loaded.outbox
                                , online = loaded.online
                                , storageReady = True
                                , route = route
                                , selectedLogbookId = selectedLogbookId
                            }
                    in
                    ( next
                    , if loaded.session /= Nothing then
                        Cmd.batch
                            [ cmdMsg RefreshLogbooks
                            , cmdMsg FlushOutbox
                            , if model.route == Login then
                                Nav.pushUrl model.key (Route.toPath Dashboard)

                              else
                                enterRoute next route
                            ]

                      else if model.route == Login then
                        Cmd.none

                      else
                        Nav.pushUrl model.key (Route.toPath Login)
                    )

                Err _ ->
                    ( { model | storageReady = True, route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    )

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
                            , email = model.loginForm.email
                            }
                    in
                    ( { model | session = Just session, busy = False, error = Nothing, route = Dashboard }
                    , Cmd.batch
                        [ Ports.storageSaveSession (Session.encode session)
                        , Nav.pushUrl model.key (Route.toPath Dashboard)
                        , cmdMsg RefreshLogbooks
                        , cmdMsg FlushOutbox
                        ]
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Identifiants invalides.", route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    )

        RefreshLogbooks ->
            case model.session of
                Nothing ->
                    ( { model | route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    )

                Just session ->
                    ( { model | busy = True }
                    , Cloudy.listLogbooks (Session.toClient session)
                        |> Request.send GotLogbooks
                    )

        GotLogbooks result ->
            case result of
                Ok books ->
                    let
                        selected =
                            case model.selectedLogbookId of
                                Just id ->
                                    if List.any (\book -> book.id == id) books then
                                        Just id

                                    else
                                        List.head books |> Maybe.map .id

                                Nothing ->
                                    List.head books |> Maybe.map .id

                        refresh =
                            case ( model.route, selected ) of
                                ( Dashboard, Just id ) ->
                                    Cmd.batch [ cmdMsg (RefreshLogs id), cmdMsg (RefreshAircraft id) ]

                                _ ->
                                    Cmd.none
                    in
                    ( { model | logbooks = books, busy = False, selectedLogbookId = selected }
                    , Cmd.batch [ Ports.storageSaveLogbooks (Persisted.encodeLogbooks books), refresh ]
                    )

                Err err ->
                    if unauthorized err then
                        update Logout { model | busy = False }

                    else
                        ( { model | busy = False, error = Just "Impossible de charger les carnets." }, Cmd.none )

        SelectLogbook id ->
            update (MenuLogbookChanged (String.fromInt id)) model

        BackToLogbooks ->
            ( model, Nav.pushUrl model.key (Route.toPath Dashboard) )

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
                    case model.session of
                        Nothing ->
                            ( { model | route = Login }
                            , Nav.pushUrl model.key (Route.toPath Login)
                            )

                        Just _ ->
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
                    if unauthorized err then
                        update Logout { model | busy = False }

                    else
                        ( { model
                            | busy = False
                            , error =
                                if showsFlights model.route logbookId then
                                    Just (httpErrorMessage "vols" err)

                                else
                                    model.error
                          }
                        , Cmd.none
                        )

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
                    ( { model | editor = Just { logbookId = logbookId, category = category, localLog = localLog, dateDraft = Nothing, uploadingFile = False, pendingFile = Nothing }, pendingNew = Nothing }
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
                    ( { model | editor = Just { logbookId = logbookId, category = localLog.category, localLog = localLog, dateDraft = Nothing, uploadingFile = False, pendingFile = Nothing } }
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

        GotLogFile file ->
            case ( model.session, model.editor ) of
                ( Just session, Just editor ) ->
                    case editor.localLog.serverId of
                        Just serverId ->
                            ( { model | editor = Just { editor | uploadingFile = True, pendingFile = Nothing }, error = Nothing }
                            , Cloudy.uploadLogFile (Session.toClient session) editor.logbookId serverId file
                                |> Request.send GotLogFileUpload
                            )

                        Nothing ->
                            saveNewLogThenUpload file editor model

                _ ->
                    ( { model | route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    )

        GotLogFileUpload result ->
            case model.editor of
                Nothing ->
                    ( model, Cmd.none )

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
                                    Dict.insert key (upsertLocal existing next) model.logs
                            in
                            ( { model | editor = Just { editor | localLog = next, uploadingFile = False }, logs = logs, error = Nothing }
                            , persistLogs logs
                            )

                        Err err ->
                            if unauthorized err then
                                update Logout { model | busy = False }

                            else
                                ( { model | editor = Just { editor | uploadingFile = False }, error = Just "Impossible d'envoyer la photo." }, Cmd.none )

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
                Nav.pushUrl model.key (Route.toPath Dashboard)

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

                        ( editor, uploadCmd ) =
                            uploadPendingFile clientId serverId entry model
                    in
                    ( { model | logs = logs, outbox = outbox, busy = False, editor = editor }
                    , Cmd.batch
                        [ persistLogs logs
                        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
                        , uploadCmd
                        , cmdMsg FlushOutbox
                        ]
                    )

                Err _ ->
                    ( { model | busy = False, error = Just "Synchronisation interrompue.", editor = clearPendingFile model.editor }
                    , Cmd.none
                    )

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

        ToggleNav ->
            ( { model | navOpen = not model.navOpen }, Cmd.none )

        CloseNav ->
            ( { model | navOpen = False }, Cmd.none )

        MenuLogbookChanged raw ->
            case String.toInt raw of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    let
                        route =
                            case model.route of
                                Logs _ ->
                                    Logs id

                                EditorNew _ ->
                                    Logs id

                                EditorEdit _ _ ->
                                    Logs id

                                Aircraft _ ->
                                    Aircraft id

                                AircraftNew _ ->
                                    Aircraft id

                                AircraftEdit _ _ ->
                                    Aircraft id

                                _ ->
                                    model.route

                        next =
                            { model | selectedLogbookId = Just id, navOpen = False }
                    in
                    if route == model.route then
                        ( next, Cmd.batch [ cmdMsg (RefreshLogs id), cmdMsg (RefreshAircraft id) ] )

                    else
                        ( next, Nav.pushUrl model.key (Route.toPath route) )

        OpenDashboard ->
            ( { model | navOpen = False }
            , if model.route == Dashboard then
                case chosenLogbook model Dashboard of
                    Just id ->
                        Cmd.batch [ cmdMsg (RefreshLogs id), cmdMsg (RefreshAircraft id) ]

                    Nothing ->
                        Cmd.none

              else
                Nav.pushUrl model.key (Route.toPath Dashboard)
            )

        OpenFlights ->
            openLogbookRoute model Logs

        OpenAircraft ->
            openLogbookRoute model Aircraft

        RefreshAircraft logbookId ->
            case model.session of
                Just session ->
                    ( model
                    , Cloudy.listAircraft (Session.toClient session) logbookId
                        |> Request.send (GotAircraft logbookId)
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotAircraft logbookId result ->
            case result of
                Ok list ->
                    let
                        aircraft =
                            Dict.insert (String.fromInt logbookId) list model.aircraft

                        opened =
                            case model.pendingAircraft of
                                Just ( pendingId, aircraftId ) ->
                                    if pendingId == logbookId then
                                        case List.filter (\item -> item.id == aircraftId) list |> List.head of
                                            Just found ->
                                                { editor = Just (editorFrom logbookId found), pending = Nothing, error = Nothing, cmd = Cmd.none }

                                            Nothing ->
                                                { editor = Nothing, pending = Nothing, error = Just "Aéronef introuvable.", cmd = Nav.pushUrl model.key (Route.toPath (Aircraft logbookId)) }

                                    else
                                        { editor = model.aircraftEditor, pending = model.pendingAircraft, error = model.error, cmd = Cmd.none }

                                Nothing ->
                                    { editor = model.aircraftEditor, pending = Nothing, error = model.error, cmd = Cmd.none }
                    in
                    ( { model | aircraft = aircraft, aircraftEditor = opened.editor, pendingAircraft = opened.pending, error = opened.error }, opened.cmd )

                Err err ->
                    if unauthorized err then
                        update Logout { model | busy = False }

                    else
                        ( { model
                            | pendingAircraft = Nothing
                            , error =
                                if aircraftRoute model.route || model.pendingAircraft /= Nothing then
                                    Just (httpErrorMessage "aéronefs" err)

                                else
                                    model.error
                          }
                        , Cmd.none
                        )

        StartNewAircraft logbookId ->
            ( { model
                | aircraftEditor =
                    Just
                        { logbookId = logbookId
                        , aircraftId = Nothing
                        , registration = ""
                        , aircraftType = ""
                        , airport = ""
                        , confirmingDelete = False
                        }
                , pendingAircraft = Nothing
                , selectedLogbookId = Just logbookId
                , navOpen = False
                , error = Nothing
              }
            , if model.route == AircraftNew logbookId then
                Cmd.none

              else
                Nav.pushUrl model.key (Route.toPath (AircraftNew logbookId))
            )

        OpenAircraftEditor logbookId aircraftId ->
            let
                navigate =
                    if model.route == AircraftEdit logbookId aircraftId then
                        Cmd.none

                    else
                        Nav.pushUrl model.key (Route.toPath (AircraftEdit logbookId aircraftId))
            in
            case findAircraft model logbookId aircraftId of
                Just found ->
                    ( { model | aircraftEditor = Just (editorFrom logbookId found), pendingAircraft = Nothing, selectedLogbookId = Just logbookId, navOpen = False, error = Nothing }
                    , Cmd.batch [ navigate, cmdMsg (RefreshAircraft logbookId) ]
                    )

                Nothing ->
                    ( { model | pendingAircraft = Just ( logbookId, aircraftId ), selectedLogbookId = Just logbookId, navOpen = False }
                    , Cmd.batch [ navigate, cmdMsg (RefreshAircraft logbookId) ]
                    )

        AircraftRegistrationChanged value ->
            updateAircraft (\editor -> { editor | registration = value }) model

        AircraftTypeChanged value ->
            updateAircraft (\editor -> { editor | aircraftType = value }) model

        AircraftAirportChanged value ->
            updateAircraft (\editor -> { editor | airport = value }) model

        SaveAircraft ->
            case ( model.session, model.aircraftEditor ) of
                ( Just session, Just editor ) ->
                    if String.trim editor.registration == "" then
                        ( { model | error = Just "L'immatriculation est obligatoire." }, Cmd.none )

                    else if not model.online then
                        ( { model | error = Just "Connexion requise pour enregistrer un aéronef." }, Cmd.none )

                    else
                        let
                            payload =
                                { registration = String.trim editor.registration
                                , aircraftType = optionalText editor.aircraftType
                                , airport = optionalText editor.airport
                                }

                            client =
                                Session.toClient session

                            request =
                                case editor.aircraftId of
                                    Nothing ->
                                        Cloudy.createAircraft client editor.logbookId payload

                                    Just aircraftId ->
                                        Cloudy.replaceAircraft client editor.logbookId aircraftId payload
                        in
                        ( { model | busy = True, error = Nothing }
                        , request |> Request.send (GotSavedAircraft editor.logbookId)
                        )

                _ ->
                    ( model, Cmd.none )

        GotSavedAircraft logbookId result ->
            case result of
                Ok aircraft ->
                    let
                        key =
                            String.fromInt logbookId

                        list =
                            Dict.get key model.aircraft |> Maybe.withDefault []
                    in
                    ( { model | aircraft = Dict.insert key (upsertAircraft list aircraft) model.aircraft, busy = False, aircraftEditor = Nothing, error = Nothing }
                    , Nav.pushUrl model.key (Route.toPath (Aircraft logbookId))
                    )

                Err err ->
                    if unauthorized err then
                        update Logout { model | busy = False }

                    else
                        ( { model | busy = False, error = Just (httpErrorMessage "aéronefs" err) }, Cmd.none )

        AskDeleteAircraft ->
            case model.aircraftEditor of
                Just editor ->
                    ( { model | aircraftEditor = Just { editor | confirmingDelete = True } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeleteAircraft ->
            case ( model.session, model.aircraftEditor, model.online ) of
                ( Just session, Just editor, True ) ->
                    case editor.aircraftId of
                        Just aircraftId ->
                            ( { model | busy = True, error = Nothing }
                            , Cloudy.deleteAircraft (Session.toClient session) editor.logbookId aircraftId
                                |> Request.sendEmpty (GotDeletedAircraft editor.logbookId aircraftId)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                ( Just _, Just _, False ) ->
                    ( { model | error = Just "Connexion requise pour supprimer un aéronef." }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotDeletedAircraft logbookId aircraftId result ->
            case result of
                Ok () ->
                    let
                        key =
                            String.fromInt logbookId

                        list =
                            Dict.get key model.aircraft
                                |> Maybe.withDefault []
                                |> List.filter (\item -> item.id /= aircraftId)
                    in
                    ( { model | aircraft = Dict.insert key list model.aircraft, busy = False, aircraftEditor = Nothing, error = Nothing }
                    , Nav.pushUrl model.key (Route.toPath (Aircraft logbookId))
                    )

                Err err ->
                    if unauthorized err then
                        update Logout { model | busy = False }

                    else
                        ( { model | busy = False, error = Just (httpErrorMessage "aéronefs" err) }, Cmd.none )

        Logout ->
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
              }
            , Cmd.batch
                [ Ports.storageSaveSession Encode.null
                , Ports.storageSaveLogbooks (Persisted.encodeLogbooks [])
                , Ports.storageSaveLogs (StorageCodec.encodeLogs Dict.empty)
                , Ports.storageSaveOutbox (StorageCodec.encodeOutbox [])
                , Nav.pushUrl model.key (Route.toPath Login)
                ]
            )


enterRoute : Model -> Route -> Cmd Msg
enterRoute model route =
    case route of
        Logs id ->
            Cmd.batch [ cmdMsg (RefreshLogs id), cmdMsg (RefreshAircraft id) ]

        Dashboard ->
            case chosenLogbook model Dashboard of
                Just id ->
                    Cmd.batch [ cmdMsg (RefreshLogs id), cmdMsg (RefreshAircraft id) ]

                Nothing ->
                    Cmd.none

        Aircraft id ->
            cmdMsg (RefreshAircraft id)

        EditorEdit id clientId ->
            Cmd.batch
                [ case model.editor of
                    Just editor ->
                        if editor.logbookId == id && editor.localLog.clientId == clientId then
                            Cmd.none

                        else
                            cmdMsg (EditLog id clientId)

                    Nothing ->
                        cmdMsg (EditLog id clientId)
                , cmdMsg (RefreshAircraft id)
                ]

        EditorNew id ->
            Cmd.batch
                [ if model.editor == Nothing then
                    cmdMsg (StartNewLog id)

                  else
                    Cmd.none
                , cmdMsg (RefreshAircraft id)
                ]

        AircraftNew id ->
            case model.aircraftEditor of
                Just editor ->
                    if editor.logbookId == id && editor.aircraftId == Nothing then
                        Cmd.none

                    else
                        cmdMsg (StartNewAircraft id)

                Nothing ->
                    cmdMsg (StartNewAircraft id)

        AircraftEdit id aircraftId ->
            case model.aircraftEditor of
                Just editor ->
                    if editor.logbookId == id && editor.aircraftId == Just aircraftId then
                        Cmd.none

                    else
                        cmdMsg (OpenAircraftEditor id aircraftId)

                Nothing ->
                    cmdMsg (OpenAircraftEditor id aircraftId)

        Login ->
            if model.session /= Nothing then
                Nav.pushUrl model.key (Route.toPath Dashboard)

            else
                Cmd.none

        Logbooks ->
            Nav.pushUrl model.key (Route.toPath Dashboard)


chosenLogbook : Model -> Route -> Maybe Int
chosenLogbook model route =
    case Route.logbookId route of
        Just id ->
            Just id

        Nothing ->
            model.selectedLogbookId


openLogbookRoute : Model -> (Int -> Route) -> ( Model, Cmd Msg )
openLogbookRoute model toRoute =
    case chosenLogbook model model.route of
        Just id ->
            let
                route =
                    toRoute id
            in
            ( { model | navOpen = False, selectedLogbookId = Just id }
            , if model.route == route then
                enterRoute model route

              else
                Nav.pushUrl model.key (Route.toPath route)
            )

        Nothing ->
            ( { model | navOpen = False }, Nav.pushUrl model.key (Route.toPath Dashboard) )


showsFlights : Route -> Int -> Bool
showsFlights route logbookId =
    case route of
        Logs id ->
            id == logbookId

        Dashboard ->
            True

        EditorNew id ->
            id == logbookId

        EditorEdit id _ ->
            id == logbookId

        _ ->
            False


aircraftRoute : Route -> Bool
aircraftRoute route =
    case route of
        Aircraft _ ->
            True

        AircraftNew _ ->
            True

        AircraftEdit _ _ ->
            True

        _ ->
            False


findAircraft : Model -> Int -> Int -> Maybe AircraftData.Aircraft
findAircraft model logbookId aircraftId =
    Dict.get (String.fromInt logbookId) model.aircraft
        |> Maybe.withDefault []
        |> List.filter (\plane -> plane.id == aircraftId)
        |> List.head


editorFrom : Int -> AircraftData.Aircraft -> AircraftEditor
editorFrom logbookId plane =
    { logbookId = logbookId
    , aircraftId = Just plane.id
    , registration = plane.registration
    , aircraftType = Maybe.withDefault "" plane.aircraftType
    , airport = Maybe.withDefault "" plane.airport
    , confirmingDelete = False
    }


updateAircraft : (AircraftEditor -> AircraftEditor) -> Model -> ( Model, Cmd Msg )
updateAircraft change model =
    ( { model
        | aircraftEditor = Maybe.map (\editor -> change { editor | confirmingDelete = False }) model.aircraftEditor
        , error = Nothing
      }
    , Cmd.none
    )


optionalText : String -> Maybe String
optionalText raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed


upsertAircraft : List AircraftData.Aircraft -> AircraftData.Aircraft -> List AircraftData.Aircraft
upsertAircraft list aircraft =
    aircraft :: List.filter (\item -> item.id /= aircraft.id) list


saveNewLogThenUpload : File.File -> EditorState -> Model -> ( Model, Cmd Msg )
saveNewLogThenUpload file editor model =
    let
        logbookKey =
            String.fromInt editor.logbookId

        existing =
            Dict.get logbookKey model.logs |> Maybe.withDefault []

        ( localLog, savedOutbox ) =
            saveLocalLog editor.localLog existing model.outbox

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
            Dict.insert logbookKey (upsertLocal existing localLog) model.logs

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
        [ persistLogs logs
        , Ports.storageSaveOutbox (StorageCodec.encodeOutbox outbox)
        , if model.route == editRoute then
            Cmd.none

          else
            Nav.pushUrl model.key (Route.toPath editRoute)
        , if model.busy then
            Cmd.none

          else
            cmdMsg FlushOutbox
        ]
    )


uploadPendingFile : String -> Int -> LogEntry -> Model -> ( Maybe EditorState, Cmd Msg )
uploadPendingFile clientId serverId entry model =
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
                        ( Just { editor | localLog = localLog, pendingFile = Nothing, uploadingFile = True }
                        , Cloudy.uploadLogFile (Session.toClient session) editor.logbookId serverId file
                            |> Request.send GotLogFileUpload
                        )

                    _ ->
                        ( Just { editor | localLog = localLog, pendingFile = Nothing, uploadingFile = False }
                        , Cmd.none
                        )

            else
                ( Just editor, Cmd.none )

        Nothing ->
            ( Nothing, Cmd.none )


clearPendingFile : Maybe EditorState -> Maybe EditorState
clearPendingFile editor =
    Maybe.map (\current -> { current | pendingFile = Nothing, uploadingFile = False }) editor


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
