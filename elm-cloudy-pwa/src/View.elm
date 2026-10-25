module View exposing (view)

import Cloudy.Data.Aircraft as AircraftData
import Cloudy.Data.Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Dict
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import LocalLog
import Model exposing (Model, Msg(..))
import Route exposing (Route(..))
import Session exposing (Session)
import Stats
import View.Aircraft as AircraftView
import View.Editor as Editor
import View.Logbook as Logbook
import View.Shell as Shell exposing (Section(..))
import View.Stats as StatsView


view : Model -> Html Msg
view model =
    case model.session of
        Nothing ->
            loginView model

        Just session ->
            Shell.view (shellConfig model session)
                (div [ class "page" ]
                    [ page model
                    , maybeError model.error
                    ]
                )


page : Model -> Html Msg
page model =
    case model.route of
        Login ->
            dashboardView model

        Dashboard ->
            dashboardView model

        Logbooks ->
            dashboardView model

        Logs id ->
            logsView model id

        Aircraft id ->
            aircraftList model id

        AircraftNew id ->
            aircraftEditorView model id

        AircraftEdit id _ ->
            aircraftEditorView model id

        EditorNew id ->
            editorRoute model id

        EditorEdit id _ ->
            editorRoute model id


shellConfig : Model -> Session -> Shell.Config Msg
shellConfig model session =
    { logbooks = model.logbooks
    , selectedId = model.selectedLogbookId
    , email = session.email
    , online = model.online
    , pending = List.length model.outbox
    , open = model.navOpen
    , section = sectionOf model.route
    , onToggle = ToggleNav
    , onClose = CloseNav
    , onSelectLogbook = MenuLogbookChanged
    , onDashboard = OpenDashboard
    , onFlights = OpenFlights
    , onAircraft = OpenAircraft
    , onLogout = Logout
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


loginView : Model -> Html Msg
loginView model =
    div [ class "login-screen" ]
        [ div [ class "card login-card" ]
            [ h1 [] [ text "Connexion" ]
            , p [ class "subtitle" ] [ text "Accédez à votre carnet de vol Cloudy." ]
            , div [ class "field" ]
                [ Html.label [] [ text "E-mail" ]
                , Html.input [ Html.Attributes.value model.loginForm.email, Html.Events.onInput LoginEmailChanged ] []
                ]
            , div [ class "field" ]
                [ Html.label [] [ text "Mot de passe" ]
                , Html.input [ Html.Attributes.type_ "password", Html.Attributes.value model.loginForm.password, Html.Events.onInput LoginPasswordChanged ] []
                ]
            , Html.details
                (class "advanced"
                    :: on "toggle" (Decode.at [ "target", "open" ] Decode.bool |> Decode.map LoginAdvancedToggled)
                    :: (if model.loginForm.advancedOpen then
                            [ Html.Attributes.attribute "open" "open" ]

                        else
                            []
                       )
                )
                [ Html.summary [] [ text "Paramètres avancés" ]
                , div [ class "field" ]
                    [ Html.label [] [ text "URL de l'API" ]
                    , Html.input [ Html.Attributes.value model.loginForm.baseUrl, Html.Events.onInput LoginBaseUrlChanged ] []
                    ]
                ]
            , button [ type_ "button", class "btn", onClick LoginSubmitted, Html.Attributes.disabled model.busy ] [ text "Se connecter" ]
            , maybeError model.error
            , p [ class "hint" ]
                [ text "Les vols saisis hors ligne seront synchronisés au retour du réseau." ]
            ]
        ]


logbookList : Model -> Html Msg
logbookList model =
    div [ class "card" ]
        [ div [ class "page-head" ]
            [ p [ class "section-title" ] [ text "Carnets" ]
            , button [ type_ "button", class "btn btn-secondary", onClick RefreshLogbooks ] [ text "Actualiser" ]
            ]
        , if not model.storageReady || (model.busy && List.isEmpty model.logbooks) then
            p [] [ text "Chargement…" ]

          else if List.isEmpty model.logbooks then
            p [ class "empty" ] [ text "Aucun carnet en cache. Actualisez une fois en ligne." ]

          else
            div [ class "list" ]
                (List.map (logbookItem model) model.logbooks)
        ]


logbookItem : Model -> Logbook -> Html Msg
logbookItem model book =
    let
        pending =
            Dict.get (String.fromInt book.id) model.logs
                |> Maybe.map LocalLog.pendingCount
                |> Maybe.withDefault 0

        selected =
            model.selectedLogbookId == Just book.id
    in
    button
        [ type_ "button"
        , class
            (if selected then
                "list-item active"

             else
                "list-item"
            )
        , onClick (SelectLogbook book.id)
        ]
        [ Html.strong [] [ text book.name ]
        , Html.span []
            [ text (Shell.categoryLabel book.category)
            , if pending > 0 then
                Html.span [ class "badge badge-pending" ] [ text (String.fromInt pending ++ " en attente") ]

              else
                text ""
            ]
        ]


logsView : Model -> Int -> Html Msg
logsView model logbookId =
    let
        logs =
            Dict.get (String.fromInt logbookId) model.logs |> Maybe.withDefault []

        book =
            findBook model logbookId

        category =
            book
                |> Maybe.map .category
                |> Maybe.withDefault
                    (logs
                        |> List.head
                        |> Maybe.map .category
                        |> Maybe.withDefault PlaneDgac
                    )
    in
    div []
        [ div [ class "page-head" ]
            [ div []
                [ h2 [ class "page-title" ]
                    [ text
                        (book
                            |> Maybe.map .name
                            |> Maybe.withDefault ("Carnet " ++ String.fromInt logbookId)
                        )
                    ]
                , p [ class "subtitle" ] [ text "Vols enregistrés dans ce carnet." ]
                ]
            , div [ class "page-actions" ]
                [ button [ type_ "button", class "btn", onClick (StartNewLog logbookId) ] [ text "+ Ajouter un vol" ] ]
            ]
        , if List.isEmpty logs then
            div [ class "card" ]
                [ p [] [ text "Aucun vol en cache." ] ]

          else
            Logbook.view category logs (Dict.get (String.fromInt logbookId) model.reports) (\clientId -> EditLog logbookId clientId)
        ]


dashboardView : Model -> Html Msg
dashboardView model =
    let
        email =
            model.session |> Maybe.map .email |> Maybe.withDefault ""

        stats =
            case activeBook model of
                Nothing ->
                    div [ class "page-head" ]
                        [ h2 [ class "page-title" ] [ text ("Bonjour " ++ Shell.displayName email) ]
                        , p [ class "subtitle" ] [ text "Sélectionnez un carnet pour voir l'activité." ]
                        ]

                Just book ->
                    let
                        logs =
                            Dict.get (String.fromInt book.id) model.logs |> Maybe.withDefault []
                    in
                    StatsView.view
                        { title = "Bonjour " ++ Shell.displayName email
                        , subtitle = book.name ++ " · " ++ Shell.categoryLabel book.category
                        , summary = Stats.summarize logs (Dict.get (String.fromInt book.id) model.reports)
                        , truncated = List.length logs >= 100
                        , onNewFlight = StartNewLog book.id
                        , onAircraft = OpenAircraft
                        }
    in
    div []
        [ stats
        , logbookList model
        ]


aircraftList : Model -> Int -> Html Msg
aircraftList model logbookId =
    AircraftView.viewList (apiBaseUrl model) (aircraftOf model logbookId) (StartNewAircraft logbookId) (\plane -> OpenAircraftEditor logbookId plane.id)


aircraftEditorView : Model -> Int -> Html Msg
aircraftEditorView model logbookId =
    case model.aircraftEditor of
        Just editor ->
            if editor.logbookId == logbookId then
                AircraftView.viewForm (apiBaseUrl model)
                    editor
                    (photoOf model editor)
                    model.busy
                    { onRegistration = AircraftRegistrationChanged
                    , onType = AircraftTypeChanged
                    , onAirport = AircraftAirportChanged
                    , onSave = SaveAircraft
                    , onAskDelete = AskDeleteAircraft
                    , onDelete = DeleteAircraft
                    , onCancel = OpenAircraft
                    , onPhoto = GotAircraftPhoto
                    }

            else
                p [] [ text "Chargement…" ]

        Nothing ->
            p [] [ text "Chargement…" ]


editorRoute : Model -> Int -> Html Msg
editorRoute model logbookId =
    case model.editor of
        Just editor ->
            if editor.logbookId == logbookId then
                Editor.view (aircraftOf model logbookId)
                    editor.localLog.entry
                    { draft = editor.dateDraft
                    , onText = DateTextChanged
                    , onPicked = DatePicked
                    , onBlur = DateTextBlurred
                    }
                    { baseUrl = apiBaseUrl model
                    , uploading = editor.uploadingFile
                    , onFile = GotLogFile
                    }
                    EditorChanged
                    SaveEditor
                    CancelEditor

            else
                p [] [ text "Chargement…" ]

        Nothing ->
            p [] [ text "Chargement…" ]


activeBook : Model -> Maybe Logbook
activeBook model =
    let
        id =
            case Route.logbookId model.route of
                Just routeId ->
                    Just routeId

                Nothing ->
                    model.selectedLogbookId
    in
    Maybe.andThen (findBook model) id


findBook : Model -> Int -> Maybe Logbook
findBook model id =
    model.logbooks
        |> List.filter (\book -> book.id == id)
        |> List.head


aircraftOf : Model -> Int -> List AircraftData.Aircraft
aircraftOf model logbookId =
    Dict.get (String.fromInt logbookId) model.aircraft
        |> Maybe.withDefault []
        |> List.sortBy (\plane -> String.toUpper plane.registration)


apiBaseUrl : Model -> String
apiBaseUrl model =
    model.session
        |> Maybe.map .baseUrl
        |> Maybe.withDefault "https://cloudy.fr/api/v1"


photoOf : Model -> Model.AircraftEditor -> Maybe String
photoOf model editor =
    editor.aircraftId
        |> Maybe.andThen
            (\aircraftId ->
                aircraftOf model editor.logbookId
                    |> List.filter (\plane -> plane.id == aircraftId)
                    |> List.head
            )
        |> Maybe.andThen .photoUrl


maybeError : Maybe String -> Html Msg
maybeError err =
    case err of
        Just message ->
            p [ class "error" ] [ text message ]

        Nothing ->
            text ""
