module Update.RouteEffects exposing (effectsForRoute)

import Effect exposing (Effect(..))
import Model exposing (Model)
import Route exposing (Route(..))
import Update.Support as Support


effectsForRoute : Model -> Route -> List Effect
effectsForRoute model route =
    case route of
        Logs id ->
            [ RefreshLogs id, RefreshAircraft id ]

        Dashboard ->
            case Support.chosenLogbook model Dashboard of
                Just id ->
                    [ RefreshLogs id, RefreshAircraft id ]

                Nothing ->
                    []

        Aircraft id ->
            [ RefreshAircraft id ]

        EditorEdit id clientId ->
            (if shouldEditLog model id clientId then
                [ EditLog id clientId ]

             else
                []
            )
                ++ [ RefreshAircraft id ]

        EditorNew id ->
            (if model.editor == Nothing then
                [ StartNewLog id ]

             else
                []
            )
                ++ [ RefreshAircraft id ]

        AircraftNew id ->
            if shouldStartNewAircraft model id then
                [ StartNewAircraft id ]

            else
                []

        AircraftEdit id aircraftId ->
            if shouldOpenAircraftEditor model id aircraftId then
                [ OpenAircraftEditor id aircraftId ]

            else
                []

        Login ->
            []

        Logbooks ->
            []


shouldEditLog : Model -> Int -> String -> Bool
shouldEditLog model logbookId clientId =
    case model.editor of
        Just editor ->
            not (editor.logbookId == logbookId && editor.localLog.clientId == clientId)

        Nothing ->
            True


shouldStartNewAircraft : Model -> Int -> Bool
shouldStartNewAircraft model logbookId =
    case model.aircraftEditor of
        Just editor ->
            not (editor.logbookId == logbookId && editor.aircraftId == Nothing)

        Nothing ->
            True


shouldOpenAircraftEditor : Model -> Int -> Int -> Bool
shouldOpenAircraftEditor model logbookId aircraftId =
    case model.aircraftEditor of
        Just editor ->
            not (editor.logbookId == logbookId && editor.aircraftId == Just aircraftId)

        Nothing ->
            True
