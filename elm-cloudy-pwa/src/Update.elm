module Update exposing (Msg(..), requestClock, update)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect(..))
import Json.Decode
import Model exposing (Model, init)
import Persisted
import Ports
import Route exposing (Route(..))
import Stats
import Task
import Time
import Update.Aircraft as Aircraft
import Update.Dashboard as Dashboard
import Update.Editor as Editor
import Update.Flights as Flights
import Update.Login as Login
import Update.Menu as Menu
import Update.Outbox as Outbox
import Update.RouteEffects
import Update.Support as Support
import Url exposing (Url)


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | StorageLoaded Json.Decode.Value
    | ConnectivityChanged Bool
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | DashboardMsg Dashboard.Msg
    | FlightsMsg Flights.Msg
    | EditorMsg Editor.Msg
    | AircraftMsg Aircraft.Msg
    | OutboxMsg Outbox.Msg
    | ClockReceived Time.Zone Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        wrap ( next, cmd, effects ) =
            ( next, Cmd.batch [ cmd, runEffects effects next ] )
    in
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
            wrap ( next, enterRouteCmd next route, Update.RouteEffects.effectsForRoute next route )

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
                                , logSync = loaded.logSync
                                , aircraft = loaded.aircraft
                                , outbox = loaded.outbox
                                , online = loaded.online
                                , storageReady = True
                                , route = route
                                , selectedLogbookId = selectedLogbookId
                            }

                        effects =
                            if loaded.session /= Nothing then
                                Update.RouteEffects.interactionEffects next route
                                    ++ [ RefreshLogbooks, RefreshProfile, FlushOutbox ]

                            else
                                []
                    in
                    ( next
                    , Cmd.batch
                        [ if loaded.session /= Nothing && model.route == Login then
                            Nav.pushUrl model.key (Route.toPath Dashboard)

                          else if loaded.session == Nothing && model.route /= Login then
                            Nav.pushUrl model.key (Route.toPath Login)

                          else if loaded.session /= Nothing then
                            enterRouteCmd next route

                          else
                            Cmd.none
                        , runEffects effects next
                        ]
                    )

                Err _ ->
                    ( { model | storageReady = True, route = Login }
                    , Nav.pushUrl model.key (Route.toPath Login)
                    )

        ConnectivityChanged online ->
            wrap
                ( { model | online = online }
                , Cmd.none
                , if online then
                    FlushOutbox
                        :: (case Support.chosenLogbook model model.route of
                                Just id ->
                                    [ RefreshLogs id, RefreshAircraft id ]

                                Nothing ->
                                    []
                           )

                  else
                    []
                )

        LoginMsg loginMsg ->
            let
                ( next, cmd, effects ) =
                    Login.update loginMsg model
            in
            wrap ( next, Cmd.map LoginMsg cmd, effects )

        MenuMsg menuMsg ->
            let
                ( next, cmd, effects ) =
                    Menu.update menuMsg model
            in
            wrap ( next, Cmd.map MenuMsg cmd, effects )

        DashboardMsg dashboardMsg ->
            case dashboardMsg of
                Dashboard.LogbookSelected id ->
                    update (MenuMsg (Menu.LogbookChanged (String.fromInt id))) model

                _ ->
                    let
                        ( next, cmd, effects ) =
                            Dashboard.update dashboardMsg model
                    in
                    wrap ( next, Cmd.map DashboardMsg cmd, effects )

        FlightsMsg flightsMsg ->
            let
                ( next, cmd, effects ) =
                    Flights.update flightsMsg model
            in
            wrap ( next, Cmd.map FlightsMsg cmd, effects )

        EditorMsg editorMsg ->
            let
                ( next, cmd, effects ) =
                    Editor.update editorMsg model
            in
            wrap ( next, Cmd.map EditorMsg cmd, effects )

        AircraftMsg aircraftMsg ->
            let
                ( next, cmd, effects ) =
                    Aircraft.update aircraftMsg model
            in
            wrap ( next, Cmd.map AircraftMsg cmd, effects )

        ClockReceived zone now ->
            ( { model | today = Just (dayFromZone zone now) }, Cmd.none )

        OutboxMsg outboxMsg ->
            let
                result =
                    Outbox.update outboxMsg model
            in
            wrap
                ( result.model
                , Cmd.batch [ Cmd.map OutboxMsg result.outboxCmd, Cmd.map EditorMsg result.editorCmd ]
                , result.effects
                )


enterRouteCmd : Model -> Route -> Cmd Msg
enterRouteCmd model route =
    case route of
        Login ->
            if model.session /= Nothing then
                Nav.pushUrl model.key (Route.toPath Dashboard)

            else
                Cmd.none

        Logbooks ->
            Nav.pushUrl model.key (Route.toPath Dashboard)

        Dashboard ->
            requestClock

        _ ->
            Cmd.none


runEffects : List Effect -> Model -> Cmd Msg
runEffects effects _ =
    effects
        |> List.map effectToMsg
        |> List.map cmdMsg
        |> Cmd.batch


effectToMsg : Effect -> Msg
effectToMsg effect =
    case effect of
        RefreshLogbooks ->
            DashboardMsg Dashboard.RefreshLogbooks

        RefreshProfile ->
            LoginMsg Login.RefreshProfile

        FlushOutbox ->
            OutboxMsg Outbox.Flush

        RefreshLogs id ->
            FlightsMsg (Flights.Refresh id)

        RefreshAircraft id ->
            AircraftMsg (Aircraft.Refresh id)

        StartNewLog id ->
            EditorMsg (Editor.StartNew id)

        EditLog id clientId ->
            EditorMsg (Editor.Edit id clientId)

        StartNewAircraft id ->
            AircraftMsg (Aircraft.StartNew id)

        OpenAircraftEditor id aircraftId ->
            AircraftMsg (Aircraft.OpenEditor id aircraftId)

        OpenAircraft ->
            MenuMsg Menu.OpenAircraft


cmdMsg : Msg -> Cmd Msg
cmdMsg message =
    Task.succeed message |> Task.perform identity


requestClock : Cmd Msg
requestClock =
    Time.here
        |> Task.andThen (\zone -> Task.map (ClockReceived zone) Time.now)
        |> Task.perform identity


dayFromZone : Time.Zone -> Time.Posix -> Stats.Day
dayFromZone zone now =
    { year = Time.toYear zone now
    , month = monthNumber (Time.toMonth zone now)
    , day = Time.toDay zone now
    }


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
