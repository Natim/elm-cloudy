module View.Stats exposing (Coverage(..), view)

import Html exposing (Html, button, div, h2, p, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import Stats exposing (AircraftTotal, ClassRow, Period(..), Summary, periodName, periods, shownAircraft)
import TimeFormat


type Coverage
    = Complete
    | LoadingMore
    | Incomplete


view :
    { title : String
    , subtitle : String
    , summary : Maybe Summary
    , coverage : Coverage
    , period : Period
    , periodText : String
    , onPeriod : Period -> msg
    , onNewFlight : msg
    , onAircraft : msg
    }
    -> Html msg
view config =
    let
        figures =
            case config.summary of
                Just summary ->
                    { flights = String.fromInt summary.flights
                    , minutes = maybeTime summary.minutes
                    , takeoffs = maybeCount summary.takeoffs
                    , landings = maybeCount summary.landings
                    }

                Nothing ->
                    { flights = "…", minutes = "…", takeoffs = "…", landings = "…" }
    in
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
        , div [ class "period-switch", attribute "role" "group", attribute "aria-label" "Période" ]
            (List.map (periodButton config) periods)
        , p [ class "period-caption" ] [ text config.periodText ]
        , div [ class "stat-grid" ]
            [ stat "Vols" figures.flights
            , stat "Temps de vol" figures.minutes
            , stat "Décollages" figures.takeoffs
            , stat "Atterrissages" figures.landings
            ]
        , p [ class "hint" ] [ text (caption config) ]
        , case config.summary of
            Just summary ->
                div [ class "card-split" ]
                    (aircraftCard config.period summary.byAircraft
                        :: cardStack
                            [ detailsCard summary.details
                            , classCard summary.byClass summary.unclassifiedMinutes
                            ]
                    )

            Nothing ->
                text ""
        ]


stat : String -> String -> Html msg
stat label value =
    div [ class "stat-card" ]
        [ span [ class "stat-label" ] [ text label ]
        , span [ class "stat-value" ] [ text value ]
        ]


caption : { a | coverage : Coverage } -> String
caption config =
    let
        source =
            "Chiffres calculés sur les vols enregistrés sur cet appareil."
    in
    case config.coverage of
        Complete ->
            source

        LoadingMore ->
            source ++ " La suite du carnet est en cours de chargement."

        Incomplete ->
            source ++ " La suite du carnet n'a pas pu être chargée."


periodButton : { a | period : Period, onPeriod : Period -> msg } -> Period -> Html msg
periodButton config period =
    button
        [ type_ "button"
        , class
            (if config.period == period then
                "active"

             else
                ""
            )
        , attribute "aria-pressed"
            (if config.period == period then
                "true"

             else
                "false"
            )
        , onClick (config.onPeriod period)
        ]
        [ text (periodName period) ]


aircraftCard : Period -> List AircraftTotal -> Html msg
aircraftCard period rows =
    let
        shown =
            shownAircraft rows
    in
    div [ class "card" ]
        [ p [ class "section-title" ] [ text "Par type" ]
        , if List.isEmpty rows then
            p [ class "empty" ]
                [ text
                    (case period of
                        All ->
                            "Aucun vol chargé pour ce carnet."

                        _ ->
                            "Aucun vol sur cette période."
                    )
                ]

          else if List.isEmpty shown then
            p [ class "empty" ] [ text "Aucun type n'atteint 5 heures." ]

          else
            div []
                [ div [ class "stat-list" ] (List.map aircraftRow shown)
                , if List.length shown < List.length rows then
                    p [ class "hint" ] [ text "Les types de moins de 5 h sont masqués." ]

                  else
                    text ""
                ]
        ]


aircraftRow : AircraftTotal -> Html msg
aircraftRow row =
    div [ class "stat-row" ]
        [ div []
            [ span [ class "stat-row-title" ] [ text row.aircraftType ]
            , span [ class "stat-row-meta" ]
                [ text
                    (String.fromInt row.flights
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


cardStack : List (Maybe (Html msg)) -> List (Html msg)
cardStack items =
    case List.filterMap identity items of
        [] ->
            []

        cards ->
            [ div [ class "card-stack" ] cards ]


detailsCard : List ( String, String ) -> Maybe (Html msg)
detailsCard details =
    if List.isEmpty details then
        Nothing

    else
        Just
            (div [ class "card" ]
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
            )


classCard : List ClassRow -> Int -> Maybe (Html msg)
classCard rows unclassified =
    if List.isEmpty rows && unclassified == 0 then
        Nothing

    else
        Just
            (div [ class "card" ]
                [ p [ class "section-title" ] [ text "SEP / MEP" ]
                , if List.isEmpty rows then
                    p [ class "empty" ] [ text "Aucun vol n'est marqué SEP ou MEP." ]

                  else
                    div [ class "stat-list" ]
                        (classHead :: List.map classRow rows)
                , if unclassified > 0 then
                    p [ class "hint" ]
                        [ text (TimeFormat.minutesToHhmm unclassified ++ " hors SEP et MEP.") ]

                  else
                    text ""
                ]
            )


classHead : Html msg
classHead =
    div [ class "stat-row class-head-row" ]
        [ span [] []
        , span [ class "class-values" ]
            [ span [] [ text "SEP" ]
            , span [] [ text "MEP" ]
            ]
        ]


classRow : ClassRow -> Html msg
classRow row =
    div [ class "stat-row" ]
        [ span [] [ text row.label ]
        , span [ class "class-values" ]
            [ span [ class "stat-row-value" ] [ text (showClassTime row.sep) ]
            , span [ class "stat-row-value" ] [ text (showClassTime row.mep) ]
            ]
        ]


showClassTime : Int -> String
showClassTime minutes =
    if minutes == 0 then
        "—"

    else
        TimeFormat.minutesToHhmm minutes


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
