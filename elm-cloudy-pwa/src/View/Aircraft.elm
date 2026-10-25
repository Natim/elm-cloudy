module View.Aircraft exposing (viewForm, viewList)

import Cloudy.Data.Aircraft exposing (Aircraft)
import Html exposing (Html, button, div, h2, img, input, p, span, text)
import Html.Attributes exposing (alt, class, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (AircraftEditor)


viewList : List Aircraft -> msg -> (Aircraft -> msg) -> Html msg
viewList aircraft onNew onOpen =
    div []
        [ div [ class "page-head" ]
            [ div []
                [ h2 [ class "page-title" ] [ text "Avions" ]
                , p [ class "subtitle" ] [ text "Immatriculations rattachées à ce carnet." ]
                ]
            , div [ class "page-actions" ]
                [ button [ type_ "button", class "btn", onClick onNew ] [ text "+ Ajouter un avion" ] ]
            ]
        , if List.isEmpty aircraft then
            div [ class "card empty-card" ]
                [ span [ class "empty-mark" ] [ text "✈" ]
                , p [] [ text "Aucun avion enregistré." ]
                ]

          else
            div [ class "list" ] (List.map (row onOpen) aircraft)
        ]


row : (Aircraft -> msg) -> Aircraft -> Html msg
row onOpen aircraft =
    button [ type_ "button", class "list-item aircraft-row", onClick (onOpen aircraft) ]
        [ mark aircraft
        , span [ class "aircraft-copy" ]
            [ Html.strong [] [ text aircraft.registration ]
            , span [] [ text (meta aircraft) ]
            ]
        ]


viewForm :
    AircraftEditor
    -> Maybe String
    -> Bool
    ->
        { onRegistration : String -> msg
        , onType : String -> msg
        , onAirport : String -> msg
        , onSave : msg
        , onAskDelete : msg
        , onDelete : msg
        , onCancel : msg
        }
    -> Html msg
viewForm editor photoUrl busy msgs =
    div [ class "editor" ]
        [ div [ class "card" ]
            [ h2 [ class "page-title" ]
                [ text
                    (case editor.aircraftId of
                        Just _ ->
                            "Modifier l'avion"

                        Nothing ->
                            "Nouvel avion"
                    )
                ]
            , p [ class "subtitle" ] [ text "Ces informations servent à retrouver l'appareil dans le carnet." ]
            , case photoUrl of
                Just url ->
                    img [ class "aircraft-photo-lg", src url, alt "" ] []

                Nothing ->
                    text ""
            , field "Immatriculation" editor.registration msgs.onRegistration
            , field "Type" editor.aircraftType msgs.onType
            , field "Aérodrome" editor.airport msgs.onAirport
            , div [ class "form-actions" ]
                [ button [ type_ "button", class "btn", onClick msgs.onSave, Html.Attributes.disabled busy ] [ text "Enregistrer" ]
                , button [ type_ "button", class "btn btn-secondary", onClick msgs.onCancel ] [ text "Annuler" ]
                , case editor.aircraftId of
                    Just _ ->
                        button
                            [ type_ "button"
                            , class "btn btn-danger"
                            , onClick
                                (if editor.confirmingDelete then
                                    msgs.onDelete

                                 else
                                    msgs.onAskDelete
                                )
                            , Html.Attributes.disabled busy
                            ]
                            [ text
                                (if editor.confirmingDelete then
                                    "Confirmer la suppression"

                                 else
                                    "Supprimer"
                                )
                            ]

                    Nothing ->
                        text ""
                ]
            ]
        ]


field : String -> String -> (String -> msg) -> Html msg
field labelText current onChange =
    div [ class "field" ]
        [ Html.label [] [ text labelText ]
        , input [ type_ "text", value current, onInput onChange ] []
        ]


mark : Aircraft -> Html msg
mark aircraft =
    case aircraft.photoUrl of
        Just url ->
            img [ class "aircraft-photo", src url, alt "" ] []

        Nothing ->
            span [ class "aircraft-mark" ] [ text (String.left 1 aircraft.registration) ]


meta : Aircraft -> String
meta aircraft =
    case ( aircraft.aircraftType, aircraft.airport ) of
        ( Just kind, Just airport ) ->
            kind ++ " · " ++ airport

        ( Just kind, Nothing ) ->
            kind

        ( Nothing, Just airport ) ->
            airport

        ( Nothing, Nothing ) ->
            "Type non renseigné"
