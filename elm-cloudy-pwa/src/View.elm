module View exposing (view)

import Cloudy.Data.Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Dict
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import LocalLog exposing (SyncStatus(..))
import Model exposing (Model, Msg(..))
import Route exposing (Route(..))
import View.Editor as Editor
import View.Logbook as Logbook


view : Model -> Html Msg
view model =
    div []
        [ statusBar model
        , case model.route of
            Login ->
                loginView model

            Logbooks ->
                logbooksView model

            Logs id ->
                logsView model id

            EditorNew id ->
                editorRoute model id

            EditorEdit id _ ->
                editorRoute model id
        ]


statusBar : Model -> Html Msg
statusBar model =
    p [ class "status-bar" ]
        [ text <|
            if model.online then
                if List.isEmpty model.outbox then
                    "En ligne"

                else
                    String.fromInt (List.length model.outbox) ++ " opération(s) en attente"

            else
                "Hors ligne"
        ]


loginView : Model -> Html Msg
loginView model =
    div [ class "shell" ]
        [ div [ class "card" ]
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
            , button [ class "btn", onClick LoginSubmitted, Html.Attributes.disabled model.busy ] [ text "Se connecter" ]
            , maybeError model.error
            , p [ class "hint" ]
                [ text "info — Les vols saisis hors ligne seront synchronisés au retour du réseau." ]
            ]
        ]


logbooksView : Model -> Html Msg
logbooksView model =
    div [ class "shell" ]
        [ div [ class "topbar" ]
            [ h2 [] [ text "Carnets" ]
            , button [ class "link-btn", onClick Logout ] [ text "Déconnexion" ]
            ]
        , if List.isEmpty model.logbooks then
            div [ class "card" ]
                [ p [] [ text "Aucun carnet en cache. Tirez pour actualiser une fois en ligne." ]
                , button [ class "btn btn-secondary", onClick RefreshLogbooks ] [ text "Actualiser" ]
                ]

          else
            div [ class "list" ]
                (List.map (logbookItem model) model.logbooks)
        , maybeError model.error
        ]


logbookItem : Model -> Logbook -> Html Msg
logbookItem model book =
    let
        pending =
            Dict.get (String.fromInt book.id) model.logs
                |> Maybe.map LocalLog.pendingCount
                |> Maybe.withDefault 0
    in
    button [ class "list-item", onClick (SelectLogbook book.id) ]
        [ Html.strong [] [ text book.name ]
        , Html.span []
            [ text (LogbookCategory.toString book.category)
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
    div [ class "shell shell-logbook" ]
        [ div [ class "topbar" ]
            [ button [ class "link-btn", onClick BackToLogbooks ] [ text "←" ]
            , h2 []
                [ text
                    (book
                        |> Maybe.map .name
                        |> Maybe.withDefault ("Carnet " ++ String.fromInt logbookId)
                    )
                ]
            ]
        , if List.isEmpty logs then
            div [ class "card" ]
                [ p [] [ text "Aucun vol en cache." ] ]

          else
            Logbook.view category logs (Dict.get (String.fromInt logbookId) model.reports) (\clientId -> EditLog logbookId clientId)
        , div [ class "fab" ]
            [ button [ class "btn", onClick (StartNewLog logbookId) ] [ text "Ajouter un vol" ]
            ]
        ]


editorRoute : Model -> Int -> Html Msg
editorRoute model logbookId =
    case model.editor of
        Just editor ->
            if editor.logbookId == logbookId then
                Editor.view editor.localLog.entry EditorChanged SaveEditor CancelEditor

            else
                p [ class "shell" ] [ text "Chargement…" ]

        Nothing ->
            p [ class "shell" ] [ text "Chargement…" ]


findBook : Model -> Int -> Maybe Logbook
findBook model id =
    model.logbooks
        |> List.filter (\b -> b.id == id)
        |> List.head


maybeError : Maybe String -> Html Msg
maybeError err =
    case err of
        Just message ->
            p [ class "error" ] [ text message ]

        Nothing ->
            text ""
