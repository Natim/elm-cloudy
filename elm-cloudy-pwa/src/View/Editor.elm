module View.Editor exposing (view)

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Log.BalloonDgac as BalloonDgac
import Cloudy.Data.Log.GliderDgac as GliderDgac
import Cloudy.Data.Log.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Log.UlmFfplum as UlmFfplum
import Html exposing (Html, button, div, h1, input, option, select, text, textarea)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import TimeFormat


view : LogEntry -> (LogEntry -> msg) -> msg -> msg -> Html msg
view entry onChange onSave onCancel =
    div [ class "shell" ]
        [ div [ class "card" ]
            [ h1 [] [ text "Vol" ]
            , body entry onChange
            , div [ class "fab" ]
                [ button [ class "btn", onClick onSave ] [ text "Enregistrer" ]
                , button [ class "btn btn-secondary", onClick onCancel ] [ text "Annuler" ]
                ]
            ]
        ]


body : LogEntry -> (LogEntry -> msg) -> Html msg
body entry onChange =
    case entry of
        PlaneDgacEntry log ->
            planeForm log (\l -> onChange (PlaneDgacEntry l))

        HelicopterDgacEntry log ->
            planeForm log (\l -> onChange (HelicopterDgacEntry l))

        GliderDgacEntry log ->
            gliderForm log (\l -> onChange (GliderDgacEntry l))

        PlaneFcl1080Entry log ->
            fclForm log (\l -> onChange (PlaneFcl1080Entry l))

        UlmFfplumEntry log ->
            ulmForm log (\l -> onChange (UlmFfplumEntry l))

        BalloonDgacEntry log ->
            balloonForm log (\l -> onChange (BalloonDgacEntry l))

        ParachuteCloudyEntry log ->
            parachuteForm log (\l -> onChange (ParachuteCloudyEntry l))


planeForm : PlaneDgac.Log -> (PlaneDgac.Log -> msg) -> Html msg
planeForm log onChange =
    div []
        [ field "Date (ISO)" log.flight_date (\v -> onChange { log | flight_date = v })
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


gliderForm : GliderDgac.Log -> (GliderDgac.Log -> msg) -> Html msg
gliderForm log onChange =
    div []
        [ field "Date (ISO)" log.flight_date (\v -> onChange { log | flight_date = v })
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


ulmForm : UlmFfplum.Log -> (UlmFfplum.Log -> msg) -> Html msg
ulmForm log onChange =
    div []
        [ field "Date (ISO)" log.flight_date (\v -> onChange { log | flight_date = v })
        , field "Type" log.aircraft_type (\v -> onChange { log | aircraft_type = v })
        , field "Immatriculation" log.aircraft_registration (\v -> onChange { log | aircraft_registration = v })
        , timeField "Double (HH:MM)" log.dual_minutes (\v -> onChange { log | dual_minutes = v })
        , timeField "CDB (HH:MM)" log.pilot_in_command_minutes (\v -> onChange { log | pilot_in_command_minutes = v })
        , intField "Décollages" log.takeoff_count (\v -> onChange { log | takeoff_count = v })
        , intField "Atterrissages" log.landing_count (\v -> onChange { log | landing_count = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


balloonForm : BalloonDgac.Log -> (BalloonDgac.Log -> msg) -> Html msg
balloonForm log onChange =
    div []
        [ field "Date (ISO)" log.flight_date (\v -> onChange { log | flight_date = v })
        , field "Type" log.ballon_type (\v -> onChange { log | ballon_type = v })
        , field "Immatriculation" log.ballon_imma (\v -> onChange { log | ballon_imma = v })
        , timeField "Double (HH:MM)" log.dual_minutes (\v -> onChange { log | dual_minutes = v })
        , timeField "CDB (HH:MM)" log.pilot_in_command_minutes (\v -> onChange { log | pilot_in_command_minutes = v })
        , intField "Décollages" log.takeoff_count (\v -> onChange { log | takeoff_count = v })
        , intField "Atterrissages" log.landing_count (\v -> onChange { log | landing_count = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


fclForm : PlaneFcl1080.Log -> (PlaneFcl1080.Log -> msg) -> Html msg
fclForm log onChange =
    div []
        [ field "Date (ISO)" log.flight_date (\v -> onChange { log | flight_date = v })
        , field "Départ" log.departure_place (\v -> onChange { log | departure_place = v })
        , field "Heure départ" log.departure_time (\v -> onChange { log | departure_time = v })
        , field "Arrivée" log.arrival_place (\v -> onChange { log | arrival_place = v })
        , field "Heure arrivée" log.arrival_time (\v -> onChange { log | arrival_time = v })
        , field "Type" log.aircraft_type (\v -> onChange { log | aircraft_type = v })
        , field "Immatriculation" log.aircraft_registration (\v -> onChange { log | aircraft_registration = v })
        , field "PIC" log.pilot_in_command_name (\v -> onChange { log | pilot_in_command_name = v })
        , timeField "Total vol (HH:MM)" log.total_time_flight (\v -> onChange { log | total_time_flight = v })
        , timeField "SE (HH:MM)" log.single_pilot_se (\v -> onChange { log | single_pilot_se = v })
        , timeField "ME (HH:MM)" log.single_pilot_me (\v -> onChange { log | single_pilot_me = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


parachuteForm : ParachuteCloudy.Log -> (ParachuteCloudy.Log -> msg) -> Html msg
parachuteForm log onChange =
    div []
        [ field "Date (ISO)" log.flight_date (\v -> onChange { log | flight_date = v })
        , field "Lieu" log.place (\v -> onChange { log | place = v })
        , field "Aéronef" log.aircraft (\v -> onChange { log | aircraft = v })
        , intField "Altitude sortie" log.altitude (\v -> onChange { log | altitude = v })
        , intField "Altitude ouverture" log.altitude_open (\v -> onChange { log | altitude_open = v })
        , timeField "Chute libre (HH:MM)" log.falling_time (\v -> onChange { log | falling_time = v })
        , field "Voile" log.canopy (\v -> onChange { log | canopy = v })
        , textArea "Commentaires" log.comments (\v -> onChange { log | comments = v })
        ]


field : String -> String -> (String -> msg) -> Html msg
field fieldLabel val onInputMsg =
    div [ class "field" ]
        [ Html.label [] [ text fieldLabel ]
        , input [ class "", value val, onInput onInputMsg ] []
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
