module View.Dashboard exposing (view)

import Cloudy.Data.Logbook exposing (Logbook)
import Dict
import Html exposing (Html, button, div, h2, p, span, strong, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import LocalLog
import Model exposing (Model)
import Route exposing (Route)
import Stats
import Update.Dashboard as Dashboard
import View.Shell as Shell
import View.Stats as StatsView


view : Model -> Html Dashboard.Msg
view model =
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
                        , coverage = logCoverage model book.id
                        , onNewFlight = Dashboard.NewFlight book.id
                        , onAircraft = Dashboard.OpenAircraft
                        }
    in
    div []
        [ stats
        , logbookList model
        ]


logbookList : Model -> Html Dashboard.Msg
logbookList model =
    div [ class "card" ]
        [ div [ class "page-head" ]
            [ p [ class "section-title" ] [ text "Carnets" ]
            , button [ type_ "button", class "btn btn-secondary", onClick Dashboard.RefreshLogbooks ] [ text "Actualiser" ]
            ]
        , if not model.storageReady || (model.busy && List.isEmpty model.logbooks) then
            p [] [ text "Chargement…" ]

          else if List.isEmpty model.logbooks then
            p [ class "empty" ] [ text "Aucun carnet en cache. Actualisez une fois en ligne." ]

          else
            div [ class "list" ]
                (List.map (logbookItem model) model.logbooks)
        ]


logbookItem : Model -> Logbook -> Html Dashboard.Msg
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
        , onClick (Dashboard.LogbookSelected book.id)
        ]
        [ strong [] [ text book.name ]
        , span []
            [ text (Shell.categoryLabel book.category)
            , if pending > 0 then
                span [ class "badge badge-pending" ] [ text (String.fromInt pending ++ " en attente") ]

              else
                text ""
            ]
        ]


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


logCoverage : Model -> Int -> StatsView.Coverage
logCoverage model logbookId =
    if model.logPaging == Just logbookId then
        StatsView.LoadingMore

    else if model.logsPartial == Just logbookId then
        StatsView.Incomplete

    else
        StatsView.Complete
