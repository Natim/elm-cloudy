module View.Logbook exposing (view)

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Log.BalloonDgac as BalloonDgac
import Cloudy.Data.Log.GliderDgac as GliderDgac
import Cloudy.Data.Log.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Log.UlmFfplum as UlmFfplum
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Html exposing (Html, button, div, small, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, rowspan, title)
import Html.Events exposing (onClick)
import LocalLog exposing (LocalLog, SyncStatus(..))
import LogHelpers
import LogbookFormat


view : LogbookCategory -> List LocalLog -> (String -> msg) -> Html msg
view category logs onOpen =
    let
        ( sheet, cards ) =
            case category of
                PlaneDgac ->
                    ( planeTable logs onOpen, planeCards logs onOpen )

                HelicopterDgac ->
                    ( planeTable logs onOpen, planeCards logs onOpen )

                GliderDgac ->
                    ( simpleTable gliderCols (gliderTotals logs) logs onOpen, simpleCards gliderCols (gliderTotals logs) logs onOpen )

                PlaneFcl1080 ->
                    ( fclTable logs onOpen, simpleCards fclCols (fclTotals logs) logs onOpen )

                UlmFfplum ->
                    ( simpleTable ulmCols (ulmTotals logs) logs onOpen, simpleCards ulmCols (ulmTotals logs) logs onOpen )

                BalloonDgac ->
                    ( simpleTable balloonCols (balloonTotals logs) logs onOpen, simpleCards balloonCols (balloonTotals logs) logs onOpen )

                ParachuteCloudy ->
                    ( simpleTable parachuteCols Nothing logs onOpen, simpleCards parachuteCols Nothing logs onOpen )
    in
    div []
        [ divScroll sheet
        , cards
        ]


divScroll : Html msg -> Html msg
divScroll child =
    Html.div [ class "logbook-scroll" ] [ child ]


planeTable : List LocalLog -> (String -> msg) -> Html msg
planeTable logs onOpen =
    let
        multi =
            List.any entryHasMulti logs
    in
    table [ class "logbook" ]
        [ thead [] (planeHeader multi ++ planeTotals multi logs)
        , tbody [] (List.map (planeRow multi onOpen) logs)
        ]


planeHeader : Bool -> List (Html msg)
planeHeader multi =
    [ tr []
        (identityHeaders
            ++ [ th [ colspan 2 ] [ text "Membre d'équipage" ]
               , th [ colspan 4 ] [ text "Pilote monomoteur" ]
               ]
            ++ multiGroupHeader multi
        )
    , tr []
        ([ th [ rowspan 2 ] [ text "Type" ]
         , th [ rowspan 2 ] [ text "Immat." ]
         , th [ rowspan 2 ] [ text "Jour" ]
         , th [ rowspan 2 ] [ text "Nuit" ]
         , th [ colspan 2 ] [ text "Jour" ]
         , th [ colspan 2 ] [ text "Nuit" ]
         ]
            ++ multiDayNightHeader multi
        )
    , tr []
        ([ th [] [ text "Double" ]
         , th [] [ text "CDB" ]
         , th [] [ text "Double" ]
         , th [] [ text "CDB" ]
         ]
            ++ multiLeafHeader multi
        )
    ]


identityHeaders : List (Html msg)
identityHeaders =
    [ th [ class "col-date", rowspan 3 ] [ text "Date" ]
    , th [ colspan 2 ] [ text "Aéronef" ]
    , th [ class "nature", rowspan 3 ] [ text "Fonction à bord" ]
    , th [ class "nature", rowspan 3 ] [ text "Nature du vol" ]
    ]


multiGroupHeader : Bool -> List (Html msg)
multiGroupHeader multi =
    if multi then
        [ th [ colspan 6 ] [ text "Pilote multimoteur" ] ]

    else
        []


multiDayNightHeader : Bool -> List (Html msg)
multiDayNightHeader multi =
    if multi then
        [ th [ colspan 3 ] [ text "Jour" ]
        , th [ colspan 3 ] [ text "Nuit" ]
        ]

    else
        []


multiLeafHeader : Bool -> List (Html msg)
multiLeafHeader multi =
    if multi then
        [ th [] [ text "Double" ]
        , th [] [ text "CDB" ]
        , th [] [ text "Copi" ]
        , th [] [ text "Double" ]
        , th [] [ text "CDB" ]
        , th [] [ text "Copi" ]
        ]

    else
        []


planeTotals : Bool -> List LocalLog -> List (Html msg)
planeTotals multi logs =
    if List.isEmpty logs then
        []

    else
        [ planeTotalsRow multi logs ]


planeTotalsRow : Bool -> List LocalLog -> Html msg
planeTotalsRow multi logs =
    tr [ class "totals" ]
        (List.repeat 5 (td [] [ text "" ])
            ++ minuteCells (planeTotalMinutes multi logs)
        )


planeTotalMinutes : Bool -> List LocalLog -> List Int
planeTotalMinutes multi logs =
    List.map (\pick -> sumPlane pick logs) (planeFields multi)


planeFields : Bool -> List (PlaneDgac.Log -> Int)
planeFields multi =
    [ .crew_day_minutes
    , .crew_night_minutes
    , .single_engine_day_dual_minutes
    , .single_engine_day_captain_minutes
    , .single_engine_night_dual_minutes
    , .single_engine_night_captain_minutes
    ]
        ++ (if multi then
                [ .multi_engine_day_dual_minutes
                , .multi_engine_day_captain_minutes
                , .multi_engine_day_co_pilot_minutes
                , .multi_engine_night_dual_minutes
                , .multi_engine_night_captain_minutes
                , .multi_engine_night_co_pilot_minutes
                ]

            else
                []
           )


sumPlane : (PlaneDgac.Log -> Int) -> List LocalLog -> Int
sumPlane pick logs =
    List.foldl
        (\log total ->
            case log.entry of
                PlaneDgacEntry entry ->
                    total + pick entry

                HelicopterDgacEntry entry ->
                    total + pick entry

                _ ->
                    total
        )
        0
        logs


planeCards : List LocalLog -> (String -> msg) -> Html msg
planeCards logs onOpen =
    div [ class "log-cards" ]
        (planeTotalsCard logs
            :: List.filterMap (planeCard onOpen) logs
        )


planeTotalsCard : List LocalLog -> Html msg
planeTotalsCard logs =
    if List.isEmpty logs then
        text ""

    else
        totalsCard (planeFlightParts logs)


planeFlightParts : List LocalLog -> List ( String, String )
planeFlightParts logs =
    [ ( "Équipage jour", LogbookFormat.showMinutes (sumPlane .crew_day_minutes logs) )
    , ( "Équipage nuit", LogbookFormat.showMinutes (sumPlane .crew_night_minutes logs) )
    , ( "Double jour", LogbookFormat.showMinutes (sumPlane .single_engine_day_dual_minutes logs) )
    , ( "CDB jour", LogbookFormat.showMinutes (sumPlane .single_engine_day_captain_minutes logs) )
    , ( "Double nuit", LogbookFormat.showMinutes (sumPlane .single_engine_night_dual_minutes logs) )
    , ( "CDB nuit", LogbookFormat.showMinutes (sumPlane .single_engine_night_captain_minutes logs) )
    , ( "Multi double jour", LogbookFormat.showMinutes (sumPlane .multi_engine_day_dual_minutes logs) )
    , ( "Multi CDB jour", LogbookFormat.showMinutes (sumPlane .multi_engine_day_captain_minutes logs) )
    , ( "Multi copi jour", LogbookFormat.showMinutes (sumPlane .multi_engine_day_co_pilot_minutes logs) )
    , ( "Multi double nuit", LogbookFormat.showMinutes (sumPlane .multi_engine_night_dual_minutes logs) )
    , ( "Multi CDB nuit", LogbookFormat.showMinutes (sumPlane .multi_engine_night_captain_minutes logs) )
    , ( "Multi copi nuit", LogbookFormat.showMinutes (sumPlane .multi_engine_night_co_pilot_minutes logs) )
    ]


planeCard : (String -> msg) -> LocalLog -> Maybe (Html msg)
planeCard onOpen log =
    case log.entry of
        PlaneDgacEntry entry ->
            Just (planeEntryCard onOpen log entry)

        HelicopterDgacEntry entry ->
            Just (planeEntryCard onOpen log entry)

        _ ->
            Nothing


planeEntryCard : (String -> msg) -> LocalLog -> PlaneDgac.Log -> Html msg
planeEntryCard onOpen log entry =
    button (rowAttrs "log-card" log onOpen)
        [ span [ class "log-card-top" ]
            [ span [ class "log-card-date" ] [ text (dateText log) ]
            , span [ class "log-card-reg" ] [ text (entry.aircraft_type ++ " · " ++ entry.aircraft_registration) ]
            , span [ class "log-card-times" ] (timeChips (planeEntryParts entry))
            ]
        , span [ class "log-card-line" ]
            [ span [ class "log-card-role" ] [ text entry.function_on_board ]
            , span [ class "log-card-nature" ]
                [ text (LogbookFormat.flightLabel entry.departure_airport entry.arrival_airport entry.flight_nature entry.observations) ]
            ]
        ]


planeEntryParts : PlaneDgac.Log -> List ( String, String )
planeEntryParts entry =
    [ ( "Équipage jour", LogbookFormat.showMinutes entry.crew_day_minutes )
    , ( "Équipage nuit", LogbookFormat.showMinutes entry.crew_night_minutes )
    , ( "Double jour", LogbookFormat.showMinutes entry.single_engine_day_dual_minutes )
    , ( "CDB jour", LogbookFormat.showMinutes entry.single_engine_day_captain_minutes )
    , ( "Double nuit", LogbookFormat.showMinutes entry.single_engine_night_dual_minutes )
    , ( "CDB nuit", LogbookFormat.showMinutes entry.single_engine_night_captain_minutes )
    , ( "Multi double jour", LogbookFormat.showMinutes entry.multi_engine_day_dual_minutes )
    , ( "Multi CDB jour", LogbookFormat.showMinutes entry.multi_engine_day_captain_minutes )
    , ( "Multi copi jour", LogbookFormat.showMinutes entry.multi_engine_day_co_pilot_minutes )
    , ( "Multi double nuit", LogbookFormat.showMinutes entry.multi_engine_night_dual_minutes )
    , ( "Multi CDB nuit", LogbookFormat.showMinutes entry.multi_engine_night_captain_minutes )
    , ( "Multi copi nuit", LogbookFormat.showMinutes entry.multi_engine_night_co_pilot_minutes )
    ]


totalsCard : List ( String, String ) -> Html msg
totalsCard parts =
    div [ class "log-totals" ]
        (parts
            |> List.filter (\( _, value ) -> value /= "")
            |> List.map
                (\( label, value ) ->
                    span []
                        [ small [] [ text label ]
                        , text value
                        ]
                )
        )


timeChips : List ( String, String ) -> List (Html msg)
timeChips parts =
    parts
        |> List.filter (\( _, value ) -> value /= "")
        |> List.map
            (\( label, value ) ->
                span []
                    [ text value
                    , small [] [ text label ]
                    ]
            )


simpleCards : List Col -> Maybe (List String) -> List LocalLog -> (String -> msg) -> Html msg
simpleCards cols totals logs onOpen =
    div [ class "log-cards" ]
        ((case totals of
            Just values ->
                [ totalsCard (List.map2 (\col value -> ( col.title, value )) cols values) ]

            Nothing ->
                []
         )
            ++ List.map (simpleCard cols onOpen) logs
        )


simpleCard : List Col -> (String -> msg) -> LocalLog -> Html msg
simpleCard cols onOpen log =
    button (rowAttrs "log-card" log onOpen)
        (List.filterMap (simpleCell log) cols)


planeRow : Bool -> (String -> msg) -> LocalLog -> Html msg
planeRow multi onOpen log =
    case log.entry of
        PlaneDgacEntry entry ->
            planeEntryRow multi onOpen log entry

        HelicopterDgacEntry entry ->
            planeEntryRow multi onOpen log entry

        _ ->
            tr [] []


planeEntryRow : Bool -> (String -> msg) -> LocalLog -> PlaneDgac.Log -> Html msg
planeEntryRow multi onOpen log entry =
    tr (rowAttrs "" log onOpen)
        ([ td [ class "col-date" ] [ text (dateText log) ]
         , td [] [ text entry.aircraft_type ]
         , td [] [ text entry.aircraft_registration ]
         , td [] [ text entry.function_on_board ]
         , td [ class "nature" ]
            [ text
                (LogbookFormat.flightLabel entry.departure_airport entry.arrival_airport entry.flight_nature entry.observations)
            ]
         ]
            ++ minuteCells
                ([ entry.crew_day_minutes
                 , entry.crew_night_minutes
                 , entry.single_engine_day_dual_minutes
                 , entry.single_engine_day_captain_minutes
                 , entry.single_engine_night_dual_minutes
                 , entry.single_engine_night_captain_minutes
                 ]
                    ++ (if multi then
                            [ entry.multi_engine_day_dual_minutes
                            , entry.multi_engine_day_captain_minutes
                            , entry.multi_engine_day_co_pilot_minutes
                            , entry.multi_engine_night_dual_minutes
                            , entry.multi_engine_night_captain_minutes
                            , entry.multi_engine_night_co_pilot_minutes
                            ]

                        else
                            []
                       )
                )
        )


minuteCells : List Int -> List (Html msg)
minuteCells values =
    List.map (\value -> td [ class "time" ] [ text (LogbookFormat.showMinutes value) ]) values


entryHasMulti : LocalLog -> Bool
entryHasMulti log =
    case log.entry of
        PlaneDgacEntry entry ->
            multiMinutes entry > 0

        HelicopterDgacEntry entry ->
            multiMinutes entry > 0

        _ ->
            False


multiMinutes : PlaneDgac.Log -> Int
multiMinutes entry =
    entry.multi_engine_day_dual_minutes
        + entry.multi_engine_day_captain_minutes
        + entry.multi_engine_day_co_pilot_minutes
        + entry.multi_engine_night_dual_minutes
        + entry.multi_engine_night_captain_minutes
        + entry.multi_engine_night_co_pilot_minutes


type alias Col =
    { title : String
    , className : String
    , cell : LocalLog -> String
    }


simpleTable : List Col -> Maybe (List String) -> List LocalLog -> (String -> msg) -> Html msg
simpleTable cols totals logs onOpen =
    table [ class "logbook" ]
        [ thead []
            (tr [] (List.map (\col -> th [ class col.className ] [ text col.title ]) cols)
                :: (case totals of
                        Just values ->
                            [ tr [ class "totals" ]
                                (List.map (\value -> td [ class "time" ] [ text value ]) values)
                            ]

                        Nothing ->
                            []
                   )
            )
        , tbody [] (List.map (simpleRow cols onOpen) logs)
        ]


simpleRow : List Col -> (String -> msg) -> LocalLog -> Html msg
simpleRow cols onOpen log =
    tr (rowAttrs "" log onOpen)
        (List.map
            (\col ->
                td [ class col.className ] [ text (col.cell log) ]
            )
            cols
        )


rowAttrs : String -> LocalLog -> (String -> msg) -> List (Html.Attribute msg)
rowAttrs extra log onOpen =
    [ class
        (String.trim
            (extra
                ++ (if log.syncStatus == Synced then
                        ""

                    else
                        " pending"
                   )
            )
        )
    , title (LocalLog.statusLabel log.syncStatus)
    , onClick (onOpen log.clientId)
    ]


simpleCell : LocalLog -> Col -> Maybe (Html msg)
simpleCell log col =
    let
        value =
            col.cell log
    in
    if value == "" then
        Nothing

    else if col.className == "col-date" then
        Just (span [ class "log-card-date" ] [ text value ])

    else
        Just
            (span []
                [ small [] [ text col.title ]
                , text value
                ]
            )


dateText : LocalLog -> String
dateText log =
    LogbookFormat.shortDate (LogHelpers.flightDate log.entry)


gliderCols : List Col
gliderCols =
    [ dateCol
    , textCol "Type" "" (glider .aircraft_type)
    , textCol "Immat." "" (glider .aircraft_registration)
    , textCol "Fonction" "" (glider .function_on_board)
    , textCol "Nature" "nature" gliderNature
    , textCol "Double" "time" (gliderMinutes .dual_minutes)
    , textCol "CDB" "time" (gliderMinutes .pilot_in_command_minutes)
    , textCol "Instr." "time" (gliderMinutes .instructor_minutes)
    ]


glider : (GliderDgac.Log -> String) -> LocalLog -> String
glider pick log =
    case log.entry of
        GliderDgacEntry entry ->
            pick entry

        _ ->
            ""


gliderNature : LocalLog -> String
gliderNature log =
    case log.entry of
        GliderDgacEntry entry ->
            LogbookFormat.flightLabel entry.departure_airport entry.arrival_airport entry.flight_nature entry.observations

        _ ->
            ""


gliderMinutes : (GliderDgac.Log -> Int) -> LocalLog -> String
gliderMinutes pick log =
    case log.entry of
        GliderDgacEntry entry ->
            LogbookFormat.showMinutes (pick entry)

        _ ->
            ""


gliderTotals : List LocalLog -> Maybe (List String)
gliderTotals logs =
    Just
        [ ""
        , ""
        , ""
        , ""
        , ""
        , LogbookFormat.showMinutes (sumGlider .dual_minutes logs)
        , LogbookFormat.showMinutes (sumGlider .pilot_in_command_minutes logs)
        , LogbookFormat.showMinutes (sumGlider .instructor_minutes logs)
        ]


sumGlider : (GliderDgac.Log -> Int) -> List LocalLog -> Int
sumGlider pick logs =
    List.foldl
        (\log total ->
            case log.entry of
                GliderDgacEntry entry ->
                    total + pick entry

                _ ->
                    total
        )
        0
        logs


ulmCols : List Col
ulmCols =
    [ dateCol
    , textCol "Type" "" (ulm .aircraft_type)
    , textCol "Immat." "" (ulm .aircraft_registration)
    , textCol "Nature" "nature" ulmNature
    , textCol "Double" "time" (ulmMinutes .dual_minutes)
    , textCol "CDB" "time" (ulmMinutes .pilot_in_command_minutes)
    ]


ulmTotals : List LocalLog -> Maybe (List String)
ulmTotals logs =
    Just
        [ ""
        , ""
        , ""
        , ""
        , LogbookFormat.showMinutes (sumUlm .dual_minutes logs)
        , LogbookFormat.showMinutes (sumUlm .pilot_in_command_minutes logs)
        ]


sumUlm : (UlmFfplum.Log -> Int) -> List LocalLog -> Int
sumUlm pick logs =
    List.foldl
        (\log total ->
            case log.entry of
                UlmFfplumEntry entry ->
                    total + pick entry

                _ ->
                    total
        )
        0
        logs


ulm : (UlmFfplum.Log -> String) -> LocalLog -> String
ulm pick log =
    case log.entry of
        UlmFfplumEntry entry ->
            pick entry

        _ ->
            ""


ulmNature : LocalLog -> String
ulmNature log =
    case log.entry of
        UlmFfplumEntry entry ->
            LogbookFormat.flightLabel entry.departure_airport entry.arrival_airport "" entry.observations

        _ ->
            ""


ulmMinutes : (UlmFfplum.Log -> Int) -> LocalLog -> String
ulmMinutes pick log =
    case log.entry of
        UlmFfplumEntry entry ->
            LogbookFormat.showMinutes (pick entry)

        _ ->
            ""


balloonCols : List Col
balloonCols =
    [ dateCol
    , textCol "Type" "" (balloon .ballon_type)
    , textCol "Immat." "" (balloon .ballon_imma)
    , textCol "Nature" "nature" balloonNature
    , textCol "Double" "time" (balloonMinutes .dual_minutes)
    , textCol "CDB" "time" (balloonMinutes .pilot_in_command_minutes)
    ]


balloon : (BalloonDgac.Log -> String) -> LocalLog -> String
balloon pick log =
    case log.entry of
        BalloonDgacEntry entry ->
            pick entry

        _ ->
            ""


balloonNature : LocalLog -> String
balloonNature log =
    case log.entry of
        BalloonDgacEntry entry ->
            LogbookFormat.flightLabel entry.departure_airport entry.arrival_airport "" entry.observations

        _ ->
            ""


balloonMinutes : (BalloonDgac.Log -> Int) -> LocalLog -> String
balloonMinutes pick log =
    case log.entry of
        BalloonDgacEntry entry ->
            LogbookFormat.showMinutes (pick entry)

        _ ->
            ""


balloonTotals : List LocalLog -> Maybe (List String)
balloonTotals logs =
    Just
        [ ""
        , ""
        , ""
        , ""
        , LogbookFormat.showMinutes (sumBalloon .dual_minutes logs)
        , LogbookFormat.showMinutes (sumBalloon .pilot_in_command_minutes logs)
        ]


sumBalloon : (BalloonDgac.Log -> Int) -> List LocalLog -> Int
sumBalloon pick logs =
    List.foldl
        (\log total ->
            case log.entry of
                BalloonDgacEntry entry ->
                    total + pick entry

                _ ->
                    total
        )
        0
        logs


fclTable : List LocalLog -> (String -> msg) -> Html msg
fclTable logs onOpen =
    table [ class "logbook" ]
        [ thead []
            (fclHeader
                ++ (case fclTotals logs of
                        Just values ->
                            [ tr [ class "totals" ]
                                (List.map (\value -> td [ class "time" ] [ text value ]) values)
                            ]

                        Nothing ->
                            []
                   )
            )
        , tbody [] (List.map (simpleRow fclCols onOpen) logs)
        ]


fclHeader : List (Html msg)
fclHeader =
    [ tr []
        [ th [ class "col-date", rowspan 2 ] [ text "Date" ]
        , th [ rowspan 2 ] [ text "Type" ]
        , th [ rowspan 2 ] [ text "Immat." ]
        , th [ rowspan 2 ] [ text "Départ" ]
        , th [ rowspan 2 ] [ text "Arrivée" ]
        , th [ rowspan 2 ] [ text "Total" ]
        , th [ colspan 2 ] [ text "Conditions" ]
        , th [ colspan 4 ] [ text "Fonction" ]
        , th [ colspan 3 ] [ text "Simulateur" ]
        ]
    , tr []
        [ th [] [ text "Nuit" ]
        , th [] [ text "IFR" ]
        , th [] [ text "CDB" ]
        , th [] [ text "Copilote" ]
        , th [] [ text "Double" ]
        , th [] [ text "Instr." ]
        , th [] [ text "Date" ]
        , th [] [ text "Type" ]
        , th [] [ text "Durée" ]
        ]
    ]


fclCols : List Col
fclCols =
    [ dateCol
    , textCol "Type" "" (fcl .aircraft_type)
    , textCol "Immat." "" (fcl .aircraft_registration)
    , textCol "Départ" "" (fcl .departure_place)
    , textCol "Arrivée" "" (fcl .arrival_place)
    , textCol "Total" "time" (fclMinutes .total_time_flight)
    , textCol "Nuit" "time" (fclMinutes .op_time_night)
    , textCol "IFR" "time" (fclMinutes .op_time_ifr)
    , textCol "CDB" "time" (fclMinutes .pilot_time_in_cmd)
    , textCol "Copilote" "time" (fclMinutes .co_pilot_minutes)
    , textCol "Double" "time" (fclMinutes .pilot_time_dual)
    , textCol "Instr." "time" (fclMinutes .pilot_time_inst)
    , textCol "Date simu" "" fclSimDate
    , textCol "Type simu" "" fclSimType
    , textCol "Simu" "time" (fclMinutes .synthetic_time)
    ]


fclTotals : List LocalLog -> Maybe (List String)
fclTotals logs =
    Just
        [ ""
        , ""
        , ""
        , ""
        , ""
        , LogbookFormat.showMinutes (sumFcl .total_time_flight logs)
        , LogbookFormat.showMinutes (sumFcl .op_time_night logs)
        , LogbookFormat.showMinutes (sumFcl .op_time_ifr logs)
        , LogbookFormat.showMinutes (sumFcl .pilot_time_in_cmd logs)
        , LogbookFormat.showMinutes (sumFcl .co_pilot_minutes logs)
        , LogbookFormat.showMinutes (sumFcl .pilot_time_dual logs)
        , LogbookFormat.showMinutes (sumFcl .pilot_time_inst logs)
        , ""
        , ""
        , LogbookFormat.showMinutes (sumFcl .synthetic_time logs)
        ]


sumFcl : (PlaneFcl1080.Log -> Int) -> List LocalLog -> Int
sumFcl pick logs =
    List.foldl
        (\log total ->
            case log.entry of
                PlaneFcl1080Entry entry ->
                    total + pick entry

                _ ->
                    total
        )
        0
        logs


fcl : (PlaneFcl1080.Log -> String) -> LocalLog -> String
fcl pick log =
    case log.entry of
        PlaneFcl1080Entry entry ->
            pick entry

        _ ->
            ""


fclMinutes : (PlaneFcl1080.Log -> Int) -> LocalLog -> String
fclMinutes pick log =
    case log.entry of
        PlaneFcl1080Entry entry ->
            LogbookFormat.showMinutes (pick entry)

        _ ->
            ""


fclSimDate : LocalLog -> String
fclSimDate log =
    case log.entry of
        PlaneFcl1080Entry entry ->
            if entry.synthetic_time == 0 && String.trim entry.synthetic_type == "" then
                ""

            else
                LogbookFormat.shortDate entry.synthetic_date

        _ ->
            ""


fclSimType : LocalLog -> String
fclSimType log =
    case log.entry of
        PlaneFcl1080Entry entry ->
            entry.synthetic_type

        _ ->
            ""


parachuteCols : List Col
parachuteCols =
    [ dateCol
    , textCol "Lieu" "" (parachute .place)
    , textCol "Aéronef" "" (parachute .aircraft)
    , textCol "Voile" "" (parachute .canopy)
    , textCol "Altitude" "time" parachuteAltitude
    ]


parachute : (ParachuteCloudy.Log -> String) -> LocalLog -> String
parachute pick log =
    case log.entry of
        ParachuteCloudyEntry entry ->
            pick entry

        _ ->
            ""


parachuteAltitude : LocalLog -> String
parachuteAltitude log =
    case log.entry of
        ParachuteCloudyEntry entry ->
            if entry.altitude == 0 then
                ""

            else
                String.fromInt entry.altitude

        _ ->
            ""


dateCol : Col
dateCol =
    { title = "Date"
    , className = "col-date"
    , cell = dateText
    }


textCol : String -> String -> (LocalLog -> String) -> Col
textCol title className cell =
    { title = title
    , className = className
    , cell = cell
    }
