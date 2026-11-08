module Update.RouteEffects exposing (effectsForRoute, interactionEffects)

import Dict
import Effect exposing (Effect(..))
import LogSync
import Model exposing (Model)
import Route exposing (Route(..))
import Update.Support as Support


effectsForRoute : Model -> Route -> List Effect
effectsForRoute model route =
    interactionEffects model route ++ cachedFill model route


interactionEffects : Model -> Route -> List Effect
interactionEffects model route =
    case route of
        EditorEdit id clientId ->
            if shouldEditLog model id clientId then
                [ EditLog id clientId ]

            else
                []

        EditorNew id ->
            if model.editor == Nothing then
                [ StartNewLog id ]

            else
                []

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

        Logs _ ->
            []

        Dashboard ->
            []

        Aircraft _ ->
            []

        Login ->
            []

        Logbooks ->
            []


cachedFill : Model -> Route -> List Effect
cachedFill model route =
    case route of
        Logs id ->
            missingLogs model id ++ missingAircraft model id

        Dashboard ->
            case Support.chosenLogbook model Dashboard of
                Just id ->
                    missingLogs model id ++ missingAircraft model id

                Nothing ->
                    []

        Aircraft id ->
            missingAircraft model id

        EditorEdit id _ ->
            missingAircraft model id

        EditorNew id ->
            missingAircraft model id

        _ ->
            []


missingLogs : Model -> Int -> List Effect
missingLogs model logbookId =
    if LogSync.isReady model.logSync model.logs logbookId then
        []

    else
        [ RefreshLogs logbookId ]


missingAircraft : Model -> Int -> List Effect
missingAircraft model logbookId =
    if Dict.member (String.fromInt logbookId) model.aircraft then
        []

    else
        [ RefreshAircraft logbookId ]


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
