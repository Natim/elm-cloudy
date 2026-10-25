module View.Common exposing (maybeError)

import Html exposing (Html, p, text)
import Html.Attributes exposing (class)


maybeError : Maybe String -> Html msg
maybeError err =
    case err of
        Just message ->
            p [ class "error" ] [ text message ]

        Nothing ->
            text ""
