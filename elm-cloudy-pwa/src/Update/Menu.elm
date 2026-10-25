module Update.Menu exposing (Msg(..), update)

import Browser.Navigation as Nav
import Effect exposing (Effect(..))
import Model exposing (Model)
import Route exposing (Route(..))
import Update.RouteEffects
import Update.Support as Support


type Msg
    = ToggleNav
    | CloseNav
    | LogbookChanged String
    | OpenDashboard
    | OpenFlights
    | OpenAircraft
    | SelectLogbook Int
    | BackToLogbooks
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg, List Effect )
update msg model =
    case msg of
        ToggleNav ->
            ( { model | navOpen = not model.navOpen }, Cmd.none, [] )

        CloseNav ->
            ( { model | navOpen = False }, Cmd.none, [] )

        SelectLogbook id ->
            update (LogbookChanged (String.fromInt id)) model

        BackToLogbooks ->
            ( model, Nav.pushUrl model.key (Route.toPath Dashboard), [] )

        LogbookChanged raw ->
            case String.toInt raw of
                Nothing ->
                    ( model, Cmd.none, [] )

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
                        ( next, Cmd.none, [ RefreshLogs id, RefreshAircraft id ] )

                    else
                        ( next, Nav.pushUrl model.key (Route.toPath route), [] )

        OpenDashboard ->
            ( { model | navOpen = False }
            , if model.route == Dashboard then
                Cmd.none

              else
                Nav.pushUrl model.key (Route.toPath Dashboard)
            , if model.route == Dashboard then
                Update.RouteEffects.effectsForRoute model Dashboard

              else
                []
            )

        OpenFlights ->
            openLogbookRoute model Logs

        OpenAircraft ->
            openLogbookRoute model Aircraft

        Logout ->
            let
                ( next, cmd ) =
                    Support.logout model
            in
            ( next, cmd, [] )


openLogbookRoute : Model -> (Int -> Route) -> ( Model, Cmd Msg, List Effect )
openLogbookRoute model toRoute =
    case Support.chosenLogbook model model.route of
        Just id ->
            let
                route =
                    toRoute id
            in
            if model.route == route then
                ( { model | navOpen = False, selectedLogbookId = Just id }
                , Cmd.none
                , Update.RouteEffects.effectsForRoute model route
                )

            else
                ( { model | navOpen = False, selectedLogbookId = Just id }
                , Nav.pushUrl model.key (Route.toPath route)
                , []
                )

        Nothing ->
            ( { model | navOpen = False }, Nav.pushUrl model.key (Route.toPath Dashboard), [] )
