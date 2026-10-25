module View.FlightForm exposing (view)

import Cloudy.Data.Aircraft as AircraftData
import Dict
import Html exposing (Html, p, text)
import Model exposing (Model)
import Update.Editor as Editor
import View.Editor as EditorView


view : Model -> Int -> Html Editor.Msg
view model logbookId =
    case model.editor of
        Just editor ->
            if editor.logbookId == logbookId then
                EditorView.view (aircraftOf model logbookId)
                    editor.localLog.entry
                    { draft = editor.dateDraft
                    , onText = Editor.DateTextChanged
                    , onPicked = Editor.DatePicked
                    , onBlur = Editor.DateTextBlurred
                    }
                    { baseUrl = apiBaseUrl model
                    , uploading = editor.uploadingFile
                    , onFile = Editor.GotLogFile
                    }
                    Editor.Changed
                    Editor.Save
                    Editor.Cancel

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
