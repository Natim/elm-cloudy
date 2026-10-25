module View.Icon exposing (aircraft, book, dashboard, flights, menu)

import Html exposing (Html)
import Html.Attributes exposing (attribute, class)


dashboard : Html msg
dashboard =
    icon "M4 4h7v7H4zM13 4h7v5h-7zM13 11h7v9h-7zM4 13h7v7H4z"


flights : Html msg
flights =
    icon "M8 3v3M16 3v3M4 9h16M6 5h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V7a2 2 0 0 1 2-2z"


book : Html msg
book =
    icon "M5 4.5h11A2.5 2.5 0 0 1 18.5 7v13H8A3 3 0 0 0 5 17V4.5zM5 4.5A2.5 2.5 0 0 0 2.5 7v12"


aircraft : Html msg
aircraft =
    icon "M21 16v-2l-8-5V3.5a1.5 1.5 0 0 0-3 0V9l-8 5v2l8-2.5V19l-2 1.5V22l3.5-1 3.5 1v-1.5L13 19v-5.5l8 2.5z"


menu : Html msg
menu =
    icon "M4 7h16M4 12h16M4 17h16"


icon : String -> Html msg
icon path =
    Html.node "svg"
        [ attribute "viewBox" "0 0 24 24"
        , attribute "fill" "none"
        , attribute "stroke" "currentColor"
        , attribute "stroke-width" "1.8"
        , attribute "stroke-linecap" "round"
        , attribute "stroke-linejoin" "round"
        , attribute "aria-hidden" "true"
        , class "icon"
        ]
        [ Html.node "path" [ attribute "d" path ] [] ]
