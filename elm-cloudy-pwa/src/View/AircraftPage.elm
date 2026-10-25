module View.AircraftPage exposing (viewEditor, viewList)

import Cloudy.Data.Aircraft as AircraftData
import Dict
import Html exposing (Html, p, text)
import Model exposing (Model)
import Update.Aircraft as Aircraft
import View.Aircraft as AircraftView


viewList : Model -> Int -> Html Aircraft.Msg
viewList model logbookId =
    AircraftView.viewList (apiBaseUrl model) (aircraftOf model logbookId) (Aircraft.StartNew logbookId) (\plane -> Aircraft.OpenEditor logbookId plane.id)


viewEditor : Model -> Int -> Html Aircraft.Msg
viewEditor model logbookId =
    case model.aircraftEditor of
        Just editor ->
            if editor.logbookId == logbookId then
                AircraftView.viewForm (apiBaseUrl model)
                    editor
                    (photoOf model editor)
                    model.busy
                    { onRegistration = Aircraft.RegistrationChanged
                    , onType = Aircraft.TypeChanged
                    , onAirport = Aircraft.AirportChanged
                    , onSave = Aircraft.Save
                    , onAskDelete = Aircraft.AskDelete
                    , onDelete = Aircraft.Delete
                    , onCancel = Aircraft.CancelForm
                    , onPhoto = Aircraft.GotPhoto
                    }

            else
                p [] [ text "Chargement…" ]

        Nothing ->
            p [] [ text "Chargement…" ]


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
