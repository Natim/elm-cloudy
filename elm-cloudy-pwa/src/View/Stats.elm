module View.Stats exposing (view)

import Html exposing (Html, button, div, h2, p, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Stats exposing (AircraftTotal, Summary)
import TimeFormat


view :
    { title : String
    , subtitle : String
    , summary : Summary
    , truncated : Bool
    , onNewFlight : msg
    , onAircraft : msg
    }
    -> Html msg
view config =
    div []
        [ div [ class "page-head" ]
            [ div []
                [ h2 [ class "page-title" ] [ text config.title ]
                , p [ class "subtitle" ] [ text config.subtitle ]
                ]
            , div [ class "page-actions" ]
                [ button [ type_ "button", class "btn", onClick config.onNewFlight ] [ text "+ Ajouter un vol" ]
                , button [ type_ "button", class "btn btn-secondary", onClick config.onAircraft ] [ text "Avions" ]
                ]
            ]
        , p [ class "section-title" ] [ text "Activité" ]
        , div [ class "stat-grid" ]
            [ stat "Vols chargés" (String.fromInt config.summary.flights)
            , stat "Temps de vol" (maybeTime config.summary.minutes)
            , stat "Décollages" (maybeCount config.summary.takeoffs)
            , stat "Atterrissages" (maybeCount config.summary.landings)
            ]
        , p [ class "hint" ] [ text (caption config) ]
        , div [ class "card-split" ]
            [ aircraftCard config.summary.byAircraft
            , detailsCard config.summary.details
            ]
        ]


stat : String -> String -> Html msg
stat label value =
    div [ class "stat-card" ]
        [ span [ class "stat-label" ] [ text label ]
        , span [ class "stat-value" ] [ text value ]
        ]


caption : { a | summary : Summary, truncated : Bool } -> String
caption config =
    let
        source =
            if config.summary.fromReport then
                "Le temps, les décollages et les atterrissages viennent du carnet complet."

            else
                "Chiffres calculés sur les vols enregistrés sur cet appareil."
    in
    if config.truncated then
        source ++ " La répartition par aéronef porte sur les 100 derniers vols chargés."

    else
        source


aircraftCard : List AircraftTotal -> Html msg
aircraftCard rows =
    div [ class "card" ]
        [ p [ class "section-title" ] [ text "Par aéronef" ]
        , if List.isEmpty rows then
            p [ class "empty" ] [ text "Aucun vol chargé pour ce carnet." ]

          else
            div [ class "stat-list" ] (List.map aircraftRow rows)
        ]


aircraftRow : AircraftTotal -> Html msg
aircraftRow row =
    div [ class "stat-row" ]
        [ div []
            [ span [ class "stat-row-title" ] [ text row.registration ]
            , span [ class "stat-row-meta" ]
                [ text
                    (row.aircraftType
                        ++ (if row.aircraftType == "" then
                                ""

                            else
                                " · "
                           )
                        ++ String.fromInt row.flights
                        ++ (if row.flights > 1 then
                                " vols"

                            else
                                " vol"
                           )
                    )
                ]
            ]
        , span [ class "stat-row-value" ] [ text (TimeFormat.minutesToHhmm row.minutes) ]
        ]


detailsCard : List ( String, String ) -> Html msg
detailsCard details =
    if List.isEmpty details then
        text ""

    else
        div [ class "card" ]
            [ p [ class "section-title" ] [ text "Détail du carnet" ]
            , div [ class "stat-list" ]
                (List.map
                    (\( label, value ) ->
                        div [ class "stat-row" ]
                            [ span [] [ text label ]
                            , span [ class "stat-row-value" ] [ text value ]
                            ]
                    )
                    details
                )
            ]


maybeTime : Maybe Int -> String
maybeTime minutes =
    case minutes of
        Just value ->
            TimeFormat.minutesToHhmm value

        Nothing ->
            "—"


maybeCount : Maybe Int -> String
maybeCount count =
    case count of
        Just value ->
            String.fromInt value

        Nothing ->
            "—"
