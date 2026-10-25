module View exposing (view)

import Html exposing (Html, div, map)
import Html.Attributes exposing (class)
import Model exposing (Model)
import Route exposing (Route(..))
import Session exposing (Session)
import Update exposing (Msg(..))
import Update.Menu as Menu
import View.AircraftPage as AircraftPage
import View.Common as Common
import View.Dashboard as Dashboard
import View.FlightForm as FlightForm
import View.Flights as Flights
import View.Login as Login
import View.Shell as Shell exposing (Section(..))


view : Model -> Html Msg
view model =
    case model.session of
        Nothing ->
            map LoginMsg (Login.view model)

        Just session ->
            Shell.view (shellConfig model session)
                (div [ class "page" ]
                    [ page model
                    , Common.maybeError model.error
                    ]
                )


page : Model -> Html Msg
page model =
    case model.route of
        Login ->
            map DashboardMsg (Dashboard.view model)

        Dashboard ->
            map DashboardMsg (Dashboard.view model)

        Logbooks ->
            map DashboardMsg (Dashboard.view model)

        Logs id ->
            map FlightsMsg (Flights.view model id)

        Aircraft id ->
            map AircraftMsg (AircraftPage.viewList model id)

        AircraftNew id ->
            map AircraftMsg (AircraftPage.viewEditor model id)

        AircraftEdit id _ ->
            map AircraftMsg (AircraftPage.viewEditor model id)

        EditorNew id ->
            map EditorMsg (FlightForm.view model id)

        EditorEdit id _ ->
            map EditorMsg (FlightForm.view model id)


shellConfig : Model -> Session -> Shell.Config Msg
shellConfig model session =
    { logbooks = model.logbooks
    , selectedId = model.selectedLogbookId
    , email = session.email
    , online = model.online
    , pending = List.length model.outbox
    , open = model.navOpen
    , section = sectionOf model.route
    , onToggle = MenuMsg Menu.ToggleNav
    , onClose = MenuMsg Menu.CloseNav
    , onSelectLogbook = MenuMsg << Menu.LogbookChanged
    , onDashboard = MenuMsg Menu.OpenDashboard
    , onFlights = MenuMsg Menu.OpenFlights
    , onAircraft = MenuMsg Menu.OpenAircraft
    , onLogout = MenuMsg Menu.Logout
    }


sectionOf : Route -> Section
sectionOf route =
    case route of
        Dashboard ->
            SecDashboard

        Login ->
            SecDashboard

        Logbooks ->
            SecDashboard

        Logs _ ->
            SecFlights

        EditorNew _ ->
            SecFlights

        EditorEdit _ _ ->
            SecFlights

        Aircraft _ ->
            SecAircraft

        AircraftNew _ ->
            SecAircraft

        AircraftEdit _ _ ->
            SecAircraft
