module View.Editor exposing (view)

import Cloudy.Data.AttachedFile exposing (AttachedFile)
import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Log.BalloonDgac as BalloonDgac
import Cloudy.Data.Log.GliderDgac as GliderDgac
import Cloudy.Data.Log.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Log.UlmFfplum as UlmFfplum
import Html exposing (Html, button, div, h1, img, input, option, select, text, textarea)
import Html.Attributes exposing (alt, checked, class, disabled, name, placeholder, selected, src, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import LogHelpers
import TimeFormat


type alias DateInput msg =
    { draft : Maybe String
    , onText : String -> msg
    , onPicked : String -> msg
    , onBlur : msg
    }


type alias FilesInput msg =
    { baseUrl : String
    , uploading : Bool
    , onPick : msg
    }


view : LogEntry -> DateInput msg -> FilesInput msg -> (LogEntry -> msg) -> msg -> msg -> Html msg
view entry dates filesInput onChange onSave onCancel =
    div [ class "editor" ]
        [ div [ class "card" ]
            [ h1 [] [ text "Vol" ]
            , body entry dates onChange
            , filesField filesInput (LogHelpers.files entry)
            , div [ class "form-actions" ]
                [ button [ type_ "button", class "btn", onClick onSave ] [ text "Enregistrer" ]
                , button [ type_ "button", class "btn btn-secondary", onClick onCancel ] [ text "Annuler" ]
                ]
            ]
        ]


body : LogEntry -> DateInput msg -> (LogEntry -> msg) -> Html msg
body entry dates onChange =
    case entry of
        PlaneDgacEntry log ->
            planeForm log dates (\l -> onChange (PlaneDgacEntry l))

        HelicopterDgacEntry log ->
            planeForm log dates (\l -> onChange (HelicopterDgacEntry l))

        GliderDgacEntry log ->
            gliderForm log dates (\l -> onChange (GliderDgacEntry l))

        PlaneFcl1080Entry log ->
            fclForm log dates (\l -> onChange (PlaneFcl1080Entry l))

        UlmFfplumEntry log ->
            ulmForm log dates (\l -> onChange (UlmFfplumEntry l))

        BalloonDgacEntry log ->
            balloonForm log dates (\l -> onChange (BalloonDgacEntry l))

        ParachuteCloudyEntry log ->
            parachuteForm log dates (\l -> onChange (ParachuteCloudyEntry l))


planeForm : PlaneDgac.Log -> DateInput msg -> (PlaneDgac.Log -> msg) -> Html msg
planeForm log dates onChange =
    div []
        [ dateField log.flight_date dates
        , field "Type" log.aircraft_type (\v -> onChange { log | aircraft_type = v })
        , field "Immatriculation" log.aircraft_registration (\v -> onChange { log | aircraft_registration = v })
        , selectField "Fonction" log.function_on_board planeFunctions (\v -> onChange { log | function_on_board = v })
        , field "Nature du vol" log.flight_nature (\v -> onChange { log | flight_nature = v })
        , field "Départ" log.departure_airport (\v -> onChange { log | departure_airport = v })
        , field "Arrivée" log.arrival_airport (\v -> onChange { log | arrival_airport = v })
        , timeField "Équipage jour (HH:MM)" log.crew_day_minutes (\v -> onChange { log | crew_day_minutes = v })
        , timeField "Équipage nuit (HH:MM)" log.crew_night_minutes (\v -> onChange { log | crew_night_minutes = v })
        , timeField "Mono CDB jour (HH:MM)" log.single_engine_day_captain_minutes (\v -> onChange { log | single_engine_day_captain_minutes = v })
        , timeField "Mono double jour (HH:MM)" log.single_engine_day_dual_minutes (\v -> onChange { log | single_engine_day_dual_minutes = v })
        , timeField "Multi CDB jour (HH:MM)" log.multi_engine_day_captain_minutes (\v -> onChange { log | multi_engine_day_captain_minutes = v })
        , timeField "IFR (min)" log.ifr (\v -> onChange { log | ifr = v })
        , intField "Atterrissages" log.landing_count (\v -> onChange { log | landing_count = v })
        , textArea "Observations" log.observations (\v -> onChange { log | observations = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


gliderForm : GliderDgac.Log -> DateInput msg -> (GliderDgac.Log -> msg) -> Html msg
gliderForm log dates onChange =
    div []
        [ dateField log.flight_date dates
        , field "Type" log.aircraft_type (\v -> onChange { log | aircraft_type = v })
        , field "Immatriculation" log.aircraft_registration (\v -> onChange { log | aircraft_registration = v })
        , selectField "Fonction" log.function_on_board gliderFunctions (\v -> onChange { log | function_on_board = v })
        , selectField "Nature" log.flight_nature gliderNature (\v -> onChange { log | flight_nature = v })
        , selectField "Lancement" log.lmode launchModes (\v -> onChange { log | lmode = v })
        , timeField "Double (HH:MM)" log.dual_minutes (\v -> onChange { log | dual_minutes = v })
        , timeField "CDB (HH:MM)" log.pilot_in_command_minutes (\v -> onChange { log | pilot_in_command_minutes = v })
        , timeField "Instructeur (HH:MM)" log.instructor_minutes (\v -> onChange { log | instructor_minutes = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


ulmForm : UlmFfplum.Log -> DateInput msg -> (UlmFfplum.Log -> msg) -> Html msg
ulmForm log dates onChange =
    div []
        [ dateField log.flight_date dates
        , field "Type" log.aircraft_type (\v -> onChange { log | aircraft_type = v })
        , field "Immatriculation" log.aircraft_registration (\v -> onChange { log | aircraft_registration = v })
        , timeField "Double (HH:MM)" log.dual_minutes (\v -> onChange { log | dual_minutes = v })
        , timeField "CDB (HH:MM)" log.pilot_in_command_minutes (\v -> onChange { log | pilot_in_command_minutes = v })
        , intField "Décollages" log.takeoff_count (\v -> onChange { log | takeoff_count = v })
        , intField "Atterrissages" log.landing_count (\v -> onChange { log | landing_count = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


balloonForm : BalloonDgac.Log -> DateInput msg -> (BalloonDgac.Log -> msg) -> Html msg
balloonForm log dates onChange =
    div []
        [ dateField log.flight_date dates
        , field "Type" log.ballon_type (\v -> onChange { log | ballon_type = v })
        , field "Immatriculation" log.ballon_imma (\v -> onChange { log | ballon_imma = v })
        , timeField "Double (HH:MM)" log.dual_minutes (\v -> onChange { log | dual_minutes = v })
        , timeField "CDB (HH:MM)" log.pilot_in_command_minutes (\v -> onChange { log | pilot_in_command_minutes = v })
        , intField "Décollages" log.takeoff_count (\v -> onChange { log | takeoff_count = v })
        , intField "Atterrissages" log.landing_count (\v -> onChange { log | landing_count = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


fclForm : PlaneFcl1080.Log -> DateInput msg -> (PlaneFcl1080.Log -> msg) -> Html msg
fclForm log dates onChange =
    let
        flightMinutes =
            if log.total_time_flight /= 0 then
                log.total_time_flight

            else
                log.single_pilot_time
    in
    div []
        [ dateField log.flight_date dates
        , div [ class "grid-2" ]
            [ field "Départ" log.departure_place (\v -> onChange { log | departure_place = v })
            , field "Heure départ" log.departure_time (\v -> onChange { log | departure_time = v })
            ]
        , div [ class "grid-2" ]
            [ field "Arrivée" log.arrival_place (\v -> onChange { log | arrival_place = v })
            , field "Heure arrivée" log.arrival_time (\v -> onChange { log | arrival_time = v })
            ]
        , div [ class "grid-2" ]
            [ field "Type" log.aircraft_type (\v -> onChange { log | aircraft_type = v })
            , field "Immatriculation" log.aircraft_registration (\v -> onChange { log | aircraft_registration = v })
            ]
        , field "PIC" log.pilot_in_command_name (\v -> onChange { log | pilot_in_command_name = v })
        , radioField "SE / ME" (singlePilotChoice log) (\choice -> onChange (setSinglePilot log choice))
        , timeField "Temps de vol (HH:MM)" flightMinutes (\v -> onChange { log | single_pilot_time = v, total_time_flight = v })
        , div [ class "grid-2" ]
            [ intField "Décollages jour" log.take_off_day (\v -> onChange { log | take_off_day = v })
            , intField "Atterrissages jour" log.landing_day (\v -> onChange { log | landing_day = v })
            ]
        , div [ class "grid-2" ]
            [ intField "Décollages nuit" log.take_off_night (\v -> onChange { log | take_off_night = v })
            , intField "Atterrissages nuit" log.landing_night (\v -> onChange { log | landing_night = v })
            ]
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


parachuteForm : ParachuteCloudy.Log -> DateInput msg -> (ParachuteCloudy.Log -> msg) -> Html msg
parachuteForm log dates onChange =
    div []
        [ dateField log.flight_date dates
        , field "Lieu" log.place (\v -> onChange { log | place = v })
        , field "Aéronef" log.aircraft (\v -> onChange { log | aircraft = v })
        , intField "Altitude sortie" log.altitude (\v -> onChange { log | altitude = v })
        , intField "Altitude ouverture" log.altitude_open (\v -> onChange { log | altitude_open = v })
        , timeField "Chute libre (HH:MM)" log.falling_time (\v -> onChange { log | falling_time = v })
        , field "Voile" log.canopy (\v -> onChange { log | canopy = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


filesField : FilesInput msg -> List AttachedFile -> Html msg
filesField filesInput attached =
    div [ class "field" ]
        [ Html.label [] [ text "Photos" ]
        , div [ class "file-list" ] (List.map (fileThumb filesInput.baseUrl) attached)
        , button
            [ type_ "button"
            , class "btn btn-secondary"
            , disabled filesInput.uploading
            , onClick filesInput.onPick
            ]
            [ text
                (if filesInput.uploading then
                    "Envoi…"

                 else
                    "Ajouter une photo"
                )
            ]
        ]


fileThumb : String -> AttachedFile -> Html msg
fileThumb baseUrl file =
    img
        [ class "file-thumb"
        , src (fileUrl baseUrl file.url)
        , alt "Photo du vol"
        ]
        []


fileUrl : String -> String -> String
fileUrl baseUrl url =
    if String.startsWith "https://" url || String.startsWith "http://" url then
        url

    else if String.startsWith "/" url then
        origin baseUrl ++ url

    else
        origin baseUrl ++ "/" ++ url


origin : String -> String
origin baseUrl =
    case String.split "/" baseUrl of
        scheme :: "" :: host :: _ ->
            scheme ++ "//" ++ host

        _ ->
            baseUrl


dateField : String -> DateInput msg -> Html msg
dateField flightDate dates =
    let
        shown =
            Maybe.withDefault (TimeFormat.formatDdMmYyyy flightDate) dates.draft
    in
    div [ class "field" ]
        [ Html.label [] [ text "Date" ]
        , div [ class "date-row" ]
            [ input
                [ type_ "text"
                , placeholder "JJ/MM/AAAA"
                , value shown
                , onInput dates.onText
                , onBlur dates.onBlur
                ]
                []
            , input
                [ type_ "date"
                , value (String.left 10 flightDate)
                , onInput dates.onPicked
                ]
                []
            ]
        ]


field : String -> String -> (String -> msg) -> Html msg
field fieldLabel val onInputMsg =
    div [ class "field" ]
        [ Html.label [] [ text fieldLabel ]
        , input [ value val, onInput onInputMsg ] []
        ]


textArea : String -> String -> (String -> msg) -> Html msg
textArea fieldLabel val onInputMsg =
    div [ class "field" ]
        [ Html.label [] [ text fieldLabel ]
        , textarea [ value val, onInput onInputMsg ] []
        ]


intField : String -> Int -> (Int -> msg) -> Html msg
intField label val onInputMsg =
    field label (String.fromInt val) (\raw -> onInputMsg (Maybe.withDefault val (String.toInt raw)))


timeField : String -> Int -> (Int -> msg) -> Html msg
timeField label minutes onInputMsg =
    field label (TimeFormat.minutesToHhmm minutes) (\raw -> onInputMsg (Maybe.withDefault minutes (TimeFormat.minutesFromHhmm raw)))


selectField : String -> String -> List ( String, String ) -> (String -> msg) -> Html msg
selectField fieldLabel current options onSelect =
    div [ class "field" ]
        [ Html.label [] [ text fieldLabel ]
        , select [ onInput onSelect ]
            (List.map (optionItem current) options)
        ]


optionItem : String -> ( String, String ) -> Html msg
optionItem current ( value_, optionLabel ) =
    option [ value value_, selected (value_ == current) ] [ text optionLabel ]


radioField : String -> String -> (String -> msg) -> Html msg
radioField fieldLabel current onSelect =
    div [ class "field" ]
        [ Html.label [] [ text fieldLabel ]
        , div [ class "radio-row" ]
            (List.map (radioItem current onSelect) singlePilotOptions)
        ]


radioItem : String -> (String -> msg) -> ( String, String ) -> Html msg
radioItem current onSelect ( value_, optionLabel ) =
    Html.label [ class "radio" ]
        [ input
            [ type_ "radio"
            , name "single-pilot"
            , value value_
            , checked (value_ == current)
            , onInput onSelect
            ]
            []
        , text optionLabel
        ]


singlePilotOptions : List ( String, String )
singlePilotOptions =
    [ ( "se", "SE" )
    , ( "me", "ME" )
    , ( "none", "Aucun" )
    ]


singlePilotChoice : PlaneFcl1080.Log -> String
singlePilotChoice log =
    if log.single_pilot_me then
        "me"

    else if log.single_pilot_se then
        "se"

    else
        "none"


setSinglePilot : PlaneFcl1080.Log -> String -> PlaneFcl1080.Log
setSinglePilot log choice =
    case choice of
        "se" ->
            { log | single_pilot_se = True, single_pilot_me = False }

        "me" ->
            { log | single_pilot_se = False, single_pilot_me = True }

        _ ->
            { log | single_pilot_se = False, single_pilot_me = False }


planeFunctions : List ( String, String )
planeFunctions =
    [ ( "EP", "Elève pilote" )
    , ( "Cdb", "Commandant de bord" )
    , ( "Pilote", "Pilote" )
    , ( "Co-pilote", "Co-pilote" )
    , ( "Inst", "Instructeur" )
    , ( "Exam", "Examinateur" )
    ]


gliderFunctions : List ( String, String )
gliderFunctions =
    planeFunctions


gliderNature : List ( String, String )
gliderNature =
    [ ( "L", "Local" )
    , ( "C", "Campagne" )
    , ( "V", "Voltige" )
    , ( "CV", "Convoyage" )
    , ( "CO", "Compétition" )
    ]


launchModes : List ( String, String )
launchModes =
    [ ( "R", "Remorqué" )
    , ( "T", "Treuil" )
    , ( "A", "Autonome" )
    ]
