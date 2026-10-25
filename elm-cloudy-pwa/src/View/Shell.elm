module View.Shell exposing (Config, Section(..), categoryLabel, displayName, view)

import Cloudy.Data.Logbook exposing (Logbook)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Html exposing (Html, aside, button, div, h1, label, option, p, select, span, text)
import Html.Attributes exposing (attribute, class, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import View.Icon as Icon


type Section
    = SecDashboard
    | SecFlights
    | SecAircraft


type alias Config msg =
    { logbooks : List Logbook
    , selectedId : Maybe Int
    , email : String
    , online : Bool
    , pending : Int
    , open : Bool
    , section : Section
    , onToggle : msg
    , onClose : msg
    , onSelectLogbook : String -> msg
    , onDashboard : msg
    , onFlights : msg
    , onAircraft : msg
    , onLogout : msg
    }


view : Config msg -> Html msg -> Html msg
view config content =
    div [ class "app" ]
        [ button
            [ type_ "button"
            , class
                (if config.open then
                    "backdrop"

                 else
                    "backdrop backdrop-hidden"
                )
            , attribute "aria-label" "Fermer le menu"
            , onClick config.onClose
            ]
            []
        , aside
            [ class
                (if config.open then
                    "sidebar open"

                 else
                    "sidebar"
                )
            ]
            [ div [ class "brand" ]
                [ span [ class "brand-mark" ] [ Icon.book ]
                , h1 [] [ text "Cloudy" ]
                ]
            , switcher config
            , Html.nav [ class "nav" ]
                [ navButton config SecDashboard config.onDashboard Icon.dashboard "Tableau de bord"
                , navButton config SecFlights config.onFlights Icon.flights "Vols"
                , navButton config SecAircraft config.onAircraft Icon.aircraft "Avions"
                ]
            , div [ class "sidebar-foot" ]
                [ p
                    [ class
                        (if config.online then
                            "status-pill"

                         else
                            "status-pill offline"
                        )
                    ]
                    [ text (statusText config) ]
                , div [ class "user" ]
                    [ span [ class "avatar" ] [ text (initials config.email) ]
                    , span [ class "user-meta" ]
                        [ span [ class "user-name" ] [ text (displayName config.email) ]
                        , span [ class "user-role" ] [ text "Pilote" ]
                        , button [ type_ "button", class "link-btn", onClick config.onLogout ] [ text "Déconnexion" ]
                        ]
                    ]
                ]
            ]
        , div [ class "main" ]
            [ div [ class "mobile-bar" ]
                [ button
                    [ type_ "button"
                    , class "icon-btn"
                    , attribute "aria-label" "Ouvrir le menu"
                    , attribute "aria-expanded"
                        (if config.open then
                            "true"

                         else
                            "false"
                        )
                    , onClick config.onToggle
                    ]
                    [ Icon.menu ]
                , span [] [ text (barTitle config) ]
                ]
            , content
            ]
        ]


switcher : Config msg -> Html msg
switcher config =
    case config.logbooks of
        [] ->
            p [ class "switcher-empty" ] [ text "Aucun carnet" ]

        books ->
            label [ class "switcher" ]
                [ span [ class "switcher-kicker" ] [ text "Carnet" ]
                , select [ onInput config.onSelectLogbook, value (selectedValue config) ]
                    (List.map (bookOption config.selectedId) books)
                ]


bookOption : Maybe Int -> Logbook -> Html msg
bookOption selectedId book =
    option
        [ value (String.fromInt book.id)
        , selected (selectedId == Just book.id)
        ]
        [ text (book.name ++ " · " ++ categoryLabel book.category) ]


navButton : Config msg -> Section -> msg -> Html msg -> String -> Html msg
navButton config section msg icon labelText =
    button
        ([ type_ "button"
         , class
            (if config.section == section then
                "nav-item active"

             else
                "nav-item"
            )
         , onClick msg
         ]
            ++ (if config.section == section then
                    [ attribute "aria-current" "page" ]

                else
                    []
               )
        )
        [ icon, text labelText ]


selectedValue : Config msg -> String
selectedValue config =
    case config.selectedId of
        Just id ->
            String.fromInt id

        Nothing ->
            ""


barTitle : Config msg -> String
barTitle config =
    config.logbooks
        |> List.filter (\book -> Just book.id == config.selectedId)
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "Cloudy"


statusText : Config msg -> String
statusText config =
    if config.online then
        if config.pending > 0 then
            String.fromInt config.pending ++ " opération(s) en attente"

        else
            "En ligne"

    else
        "Hors ligne"


displayName : String -> String
displayName email =
    case String.split "@" email of
        local :: _ ->
            case String.uncons local of
                Just ( first, rest ) ->
                    String.toUpper (String.fromChar first) ++ rest

                Nothing ->
                    "Pilote"

        [] ->
            "Pilote"


initials : String -> String
initials email =
    let
        local =
            String.split "@" email |> List.head |> Maybe.withDefault ""

        parts =
            String.split "." local |> List.filter (\part -> part /= "")
    in
    case parts of
        first :: second :: _ ->
            String.toUpper (String.left 1 first ++ String.left 1 second)

        first :: _ ->
            String.toUpper (String.left 2 first)

        [] ->
            "P"


categoryLabel : LogbookCategory -> String
categoryLabel category =
    case category of
        PlaneDgac ->
            "Avion DGAC"

        GliderDgac ->
            "Planeur DGAC"

        PlaneFcl1080 ->
            "Avion FCL.050"

        HelicopterDgac ->
            "Hélicoptère DGAC"

        UlmFfplum ->
            "ULM FFPLUM"

        BalloonDgac ->
            "Ballon DGAC"

        ParachuteCloudy ->
            "Parachute"
