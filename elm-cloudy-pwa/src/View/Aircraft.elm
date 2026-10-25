module View.Aircraft exposing (viewForm, viewList)

import Cloudy.Data.Aircraft exposing (Aircraft)
import File exposing (File)
import Html exposing (Html, button, div, h2, img, input, p, span, text)
import Html.Attributes exposing (accept, alt, class, disabled, src, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import MediaUrl
import Model exposing (AircraftEditor)


viewList : String -> List Aircraft -> msg -> (Aircraft -> msg) -> Html msg
viewList baseUrl aircraft onNew onOpen =
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
            div [ class "list" ] (List.map (row baseUrl onOpen) aircraft)
        ]


row : String -> (Aircraft -> msg) -> Aircraft -> Html msg
row baseUrl onOpen aircraft =
    button [ type_ "button", class "list-item aircraft-row", onClick (onOpen aircraft) ]
        [ mark baseUrl aircraft
        , span [ class "aircraft-copy" ]
            [ Html.strong [] [ text aircraft.registration ]
            , span [] [ text (meta aircraft) ]
            ]
        ]


viewForm :
    String
    -> AircraftEditor
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
        , onPhoto : File -> msg
        }
    -> Html msg
viewForm baseUrl editor photoUrl busy msgs =
    let
        locked =
            busy || editor.uploadingPhoto
    in
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
                    img [ class "aircraft-photo-lg", src (MediaUrl.absoluteUrl baseUrl url), alt "" ] []

                Nothing ->
                    text ""
            , photoField locked editor msgs.onPhoto
            , field "Immatriculation" editor.registration msgs.onRegistration
            , field "Type" editor.aircraftType msgs.onType
            , field "Aérodrome" editor.airport msgs.onAirport
            , div [ class "form-actions" ]
                [ button [ type_ "button", class "btn", onClick msgs.onSave, disabled locked ] [ text "Enregistrer" ]
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
                            , disabled locked
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


photoField : Bool -> AircraftEditor -> (File -> msg) -> Html msg
photoField locked editor onPhoto =
    div [ class "field" ]
        [ Html.label [] [ text "Photo" ]
        , input
            [ type_ "file"
            , class "file-input"
            , accept "image/*"
            , disabled locked
            , on "change" (Decode.map onPhoto (Decode.at [ "target", "files", "0" ] File.decoder))
            ]
            []
        , if editor.uploadingPhoto then
            p [ class "hint" ] [ text "Envoi…" ]

          else
            case editor.pendingPhoto of
                Just file ->
                    p [ class "hint" ] [ text ("Sera envoyée à l'enregistrement : " ++ File.name file) ]

                Nothing ->
                    text ""
        ]


field : String -> String -> (String -> msg) -> Html msg
field labelText current onChange =
    div [ class "field" ]
        [ Html.label [] [ text labelText ]
        , input [ type_ "text", value current, onInput onChange ] []
        ]


mark : String -> Aircraft -> Html msg
mark baseUrl aircraft =
    case aircraft.photoUrl of
        Just url ->
            img [ class "aircraft-photo", src (MediaUrl.absoluteUrl baseUrl url), alt "" ] []

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
