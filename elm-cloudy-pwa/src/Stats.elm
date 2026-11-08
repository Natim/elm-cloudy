module Stats exposing
    ( AircraftTotal
    , ClassRow
    , Day
    , Period(..)
    , Summary
    , inPeriod
    , periodName
    , periodText
    , periods
    , shownAircraft
    , summarize
    )

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Log.BalloonDgac as BalloonDgac
import Cloudy.Data.Log.GliderDgac as GliderDgac
import Cloudy.Data.Log.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Log.UlmFfplum as UlmFfplum
import Dict exposing (Dict)
import LocalLog exposing (LocalLog)
import LogHelpers
import LogbookFormat


type alias Summary =
    { flights : Int
    , minutes : Maybe Int
    , takeoffs : Maybe Int
    , landings : Maybe Int
    , details : List ( String, String )
    , byAircraft : List AircraftTotal
    , byClass : List ClassRow
    , unclassifiedMinutes : Int
    }


type alias ClassRow =
    { label : String
    , sep : Int
    , mep : Int
    }


type alias AircraftTotal =
    { aircraftType : String
    , flights : Int
    , minutes : Int
    }


summarize : List LocalLog -> Summary
summarize logs =
    let
        fromLogs =
            List.foldl addLog emptyTotals logs

        classes =
            classSplit logs
    in
    { flights = fromLogs.flights
    , minutes = Just fromLogs.minutes
    , takeoffs = Just fromLogs.takeoffs
    , landings = Just fromLogs.landings
    , details = flightDetails logs
    , byAircraft = aircraftTotals logs
    , byClass = classes.rows
    , unclassifiedMinutes = classes.unclassified
    }


emptyTotals : { flights : Int, minutes : Int, takeoffs : Int, landings : Int }
emptyTotals =
    { flights = 0, minutes = 0, takeoffs = 0, landings = 0 }


addLog : LocalLog -> { flights : Int, minutes : Int, takeoffs : Int, landings : Int } -> { flights : Int, minutes : Int, takeoffs : Int, landings : Int }
addLog log totals =
    { flights = totals.flights + 1
    , minutes = totals.minutes + blockMinutes log.entry
    , takeoffs = totals.takeoffs + takeoffCount log.entry
    , landings = totals.landings + landingCount log.entry
    }


flightDetails : List LocalLog -> List ( String, String )
flightDetails logs =
    case logs of
        [] ->
            []

        first :: _ ->
            case first.entry of
                PlaneDgacEntry _ ->
                    planeFlightDetails logs

                HelicopterDgacEntry _ ->
                    planeFlightDetails logs

                GliderDgacEntry _ ->
                    gliderFlightDetails logs

                BalloonDgacEntry _ ->
                    balloonFlightDetails logs

                PlaneFcl1080Entry _ ->
                    fclFlightDetails logs

                UlmFfplumEntry _ ->
                    ulmFlightDetails logs

                ParachuteCloudyEntry _ ->
                    parachuteFlightDetails logs


planeFlightDetails : List LocalLog -> List ( String, String )
planeFlightDetails logs =
    keep
        (List.map (summed planeMinutes logs) planeDetailFields
            ++ [ ( "IFR", countLabel (summedInt planeMinutes .ifr logs) )
               , ( "Simulateur", LogbookFormat.showMinutes (summedInt planeMinutes .simulator_minutes logs) )
               , ( "Instrument double", LogbookFormat.showMinutes (summedInt planeMinutes .instrument_day_dual_minutes logs) )
               , ( "Instrument", LogbookFormat.showMinutes (summedInt planeMinutes .instrument_pilot_minutes logs) )
               ]
        )


planeDetailFields : List ( String, PlaneDgac.Log -> Int )
planeDetailFields =
    [ ( "Équipage jour", .crew_day_minutes )
    , ( "Équipage nuit", .crew_night_minutes )
    , ( "Double jour", .single_engine_day_dual_minutes )
    , ( "CDB jour", .single_engine_day_captain_minutes )
    , ( "Double nuit", .single_engine_night_dual_minutes )
    , ( "CDB nuit", .single_engine_night_captain_minutes )
    , ( "Multi double jour", .multi_engine_day_dual_minutes )
    , ( "Multi CDB jour", .multi_engine_day_captain_minutes )
    , ( "Multi copi jour", .multi_engine_day_co_pilot_minutes )
    , ( "Multi double nuit", .multi_engine_night_dual_minutes )
    , ( "Multi CDB nuit", .multi_engine_night_captain_minutes )
    , ( "Multi copi nuit", .multi_engine_night_co_pilot_minutes )
    ]


gliderFlightDetails : List LocalLog -> List ( String, String )
gliderFlightDetails logs =
    keep
        [ ( "Double commande", LogbookFormat.showMinutes (summedInt gliderMinutes .dual_minutes logs) )
        , ( "Commandant de bord", LogbookFormat.showMinutes (summedInt gliderMinutes .pilot_in_command_minutes logs) )
        , ( "Instruction", LogbookFormat.showMinutes (summedInt gliderMinutes .instructor_minutes logs) )
        ]


balloonFlightDetails : List LocalLog -> List ( String, String )
balloonFlightDetails logs =
    keep
        [ ( "Double commande", LogbookFormat.showMinutes (summedInt balloonMinutes .dual_minutes logs) )
        , ( "Commandant de bord", LogbookFormat.showMinutes (summedInt balloonMinutes .pilot_in_command_minutes logs) )
        ]


fclFlightDetails : List LocalLog -> List ( String, String )
fclFlightDetails logs =
    keep
        (List.map (summed fclMinutes logs) fclDetailFields
            ++ [ ( "Simulateur", LogbookFormat.showMinutes (summedInt fclMinutes .synthetic_time logs) ) ]
        )


fclDetailFields : List ( String, PlaneFcl1080.Log -> Int )
fclDetailFields =
    [ ( "Temps de vol", .total_time_flight )
    , ( "Pilote seul", .single_pilot_time )
    , ( "Multipilote", .multi_pilot_time )
    , ( "Commandant de bord", .pilot_time_in_cmd )
    , ( "Double commande", .pilot_time_dual )
    , ( "Copilote", .co_pilot_minutes )
    , ( "Nuit", .op_time_night )
    , ( "IFR", .op_time_ifr )
    , ( "Instruction", .pilot_time_inst )
    ]


classSplit : List LocalLog -> { rows : List ClassRow, unclassified : Int }
classSplit logs =
    case logs of
        [] ->
            { rows = [], unclassified = 0 }

        first :: _ ->
            case first.entry of
                PlaneFcl1080Entry _ ->
                    { rows = fclClassRows logs, unclassified = fclUnclassified logs }

                PlaneDgacEntry _ ->
                    { rows = planeClassRows logs, unclassified = 0 }

                HelicopterDgacEntry _ ->
                    { rows = planeClassRows logs, unclassified = 0 }

                _ ->
                    { rows = [], unclassified = 0 }


fclClassRows : List LocalLog -> List ClassRow
fclClassRows logs =
    fclDetailFields
        |> List.map
            (\( label, field ) ->
                List.foldl (addFclClass field) { label = label, sep = 0, mep = 0 } logs
            )
        |> List.filter (\row -> row.sep > 0 || row.mep > 0)


addFclClass : (PlaneFcl1080.Log -> Int) -> LocalLog -> ClassRow -> ClassRow
addFclClass field log row =
    case log.entry of
        PlaneFcl1080Entry entry ->
            let
                minutes =
                    field entry
            in
            if entry.single_pilot_me then
                { row | mep = row.mep + minutes }

            else if entry.single_pilot_se then
                { row | sep = row.sep + minutes }

            else
                row

        _ ->
            row


fclUnclassified : List LocalLog -> Int
fclUnclassified logs =
    List.foldl
        (\log total ->
            case log.entry of
                PlaneFcl1080Entry entry ->
                    if entry.single_pilot_se || entry.single_pilot_me then
                        total

                    else
                        total + entry.total_time_flight

                _ ->
                    total
        )
        0
        logs


planeClassRows : List LocalLog -> List ClassRow
planeClassRows logs =
    [ planeClassSum "Temps de vol" planeSepMinutes planeMepMinutes logs
    , planeClassSum "Double commande" planeSepDual planeMepDual logs
    , planeClassSum "Commandant de bord" planeSepCaptain planeMepCaptain logs
    , planeClassSum "Copilote" (\_ -> 0) planeMepCoPilot logs
    , planeClassSum "Nuit" planeSepNight planeMepNight logs
    ]
        |> List.filter (\row -> row.sep > 0 || row.mep > 0)


planeClassSum : String -> (PlaneDgac.Log -> Int) -> (PlaneDgac.Log -> Int) -> List LocalLog -> ClassRow
planeClassSum label sepOf mepOf logs =
    List.foldl (addPlaneClass sepOf mepOf) { label = label, sep = 0, mep = 0 } logs


addPlaneClass : (PlaneDgac.Log -> Int) -> (PlaneDgac.Log -> Int) -> LocalLog -> ClassRow -> ClassRow
addPlaneClass sepOf mepOf log row =
    case log.entry of
        PlaneDgacEntry entry ->
            { row | sep = row.sep + sepOf entry, mep = row.mep + mepOf entry }

        HelicopterDgacEntry entry ->
            { row | sep = row.sep + sepOf entry, mep = row.mep + mepOf entry }

        _ ->
            row


planeSepMinutes : PlaneDgac.Log -> Int
planeSepMinutes log =
    planeSepDual log + planeSepCaptain log


planeMepMinutes : PlaneDgac.Log -> Int
planeMepMinutes log =
    planeMepDual log + planeMepCaptain log + planeMepCoPilot log


planeSepDual : PlaneDgac.Log -> Int
planeSepDual log =
    log.single_engine_day_dual_minutes + log.single_engine_night_dual_minutes


planeMepDual : PlaneDgac.Log -> Int
planeMepDual log =
    log.multi_engine_day_dual_minutes + log.multi_engine_night_dual_minutes


planeSepCaptain : PlaneDgac.Log -> Int
planeSepCaptain log =
    log.single_engine_day_captain_minutes + log.single_engine_night_captain_minutes


planeMepCaptain : PlaneDgac.Log -> Int
planeMepCaptain log =
    log.multi_engine_day_captain_minutes + log.multi_engine_night_captain_minutes


planeMepCoPilot : PlaneDgac.Log -> Int
planeMepCoPilot log =
    log.multi_engine_day_co_pilot_minutes + log.multi_engine_night_co_pilot_minutes


planeSepNight : PlaneDgac.Log -> Int
planeSepNight log =
    log.single_engine_night_dual_minutes + log.single_engine_night_captain_minutes


planeMepNight : PlaneDgac.Log -> Int
planeMepNight log =
    log.multi_engine_night_dual_minutes
        + log.multi_engine_night_captain_minutes
        + log.multi_engine_night_co_pilot_minutes


ulmFlightDetails : List LocalLog -> List ( String, String )
ulmFlightDetails logs =
    keep
        [ ( "Double commande", LogbookFormat.showMinutes (summedInt ulmMinutes .dual_minutes logs) )
        , ( "Commandant de bord", LogbookFormat.showMinutes (summedInt ulmMinutes .pilot_in_command_minutes logs) )
        ]


parachuteFlightDetails : List LocalLog -> List ( String, String )
parachuteFlightDetails logs =
    keep
        [ ( "Sauts", countLabel (List.length logs) )
        , ( "Chute", LogbookFormat.showMinutes (summedInt parachuteMinutes .falling_time logs) )
        ]


summed : (a -> LocalLog -> Int) -> List LocalLog -> ( String, a ) -> ( String, String )
summed pick logs ( label, field ) =
    ( label, LogbookFormat.showMinutes (summedInt pick field logs) )


summedInt : (a -> LocalLog -> Int) -> a -> List LocalLog -> Int
summedInt pick field logs =
    List.foldl (\log total -> total + pick field log) 0 logs


planeMinutes : (PlaneDgac.Log -> Int) -> LocalLog -> Int
planeMinutes pick log =
    case log.entry of
        PlaneDgacEntry entry ->
            pick entry

        HelicopterDgacEntry entry ->
            pick entry

        _ ->
            0


gliderMinutes : (GliderDgac.Log -> Int) -> LocalLog -> Int
gliderMinutes pick log =
    case log.entry of
        GliderDgacEntry entry ->
            pick entry

        _ ->
            0


balloonMinutes : (BalloonDgac.Log -> Int) -> LocalLog -> Int
balloonMinutes pick log =
    case log.entry of
        BalloonDgacEntry entry ->
            pick entry

        _ ->
            0


fclMinutes : (PlaneFcl1080.Log -> Int) -> LocalLog -> Int
fclMinutes pick log =
    case log.entry of
        PlaneFcl1080Entry entry ->
            pick entry

        _ ->
            0


ulmMinutes : (UlmFfplum.Log -> Int) -> LocalLog -> Int
ulmMinutes pick log =
    case log.entry of
        UlmFfplumEntry entry ->
            pick entry

        _ ->
            0


parachuteMinutes : (ParachuteCloudy.Log -> Int) -> LocalLog -> Int
parachuteMinutes pick log =
    case log.entry of
        ParachuteCloudyEntry entry ->
            pick entry

        _ ->
            0


aircraftTotals : List LocalLog -> List AircraftTotal
aircraftTotals logs =
    logs
        |> List.foldl insertAircraft Dict.empty
        |> Dict.values
        |> List.sortBy (\item -> ( -item.minutes, item.aircraftType ))


type Period
    = LastCompleteMonth
    | Last12Months
    | Last24Months
    | All


type alias Day =
    { year : Int
    , month : Int
    , day : Int
    }


periods : List Period
periods =
    [ LastCompleteMonth, Last12Months, Last24Months, All ]


periodName : Period -> String
periodName period =
    case period of
        LastCompleteMonth ->
            "Mois dernier"

        Last12Months ->
            "12 derniers mois"

        Last24Months ->
            "24 derniers mois"

        All ->
            "Totalité"


periodText : Period -> Day -> String
periodText period today =
    case period of
        LastCompleteMonth ->
            let
                start =
                    addMonths -1 { today | day = 1 }
            in
            capitalize (monthName start.month) ++ " " ++ String.fromInt start.year

        Last12Months ->
            rangeText (addMonths -12 today) today

        Last24Months ->
            rangeText (addMonths -24 today) today

        All ->
            "Tous les vols"


inPeriod : Period -> Day -> List LocalLog -> List LocalLog
inPeriod period today logs =
    case bounds period today of
        Nothing ->
            logs

        Just ( from, to ) ->
            List.filter (flightInRange from to) logs


bounds : Period -> Day -> Maybe ( String, String )
bounds period today =
    case period of
        LastCompleteMonth ->
            let
                start =
                    addMonths -1 { today | day = 1 }
            in
            Just ( iso start, iso { start | day = daysInMonth start.year start.month } )

        Last12Months ->
            Just ( iso (addMonths -12 today), iso today )

        Last24Months ->
            Just ( iso (addMonths -24 today), iso today )

        All ->
            Nothing


flightInRange : String -> String -> LocalLog -> Bool
flightInRange from to log =
    let
        date =
            String.left 10 (LogHelpers.flightDate log.entry)
    in
    String.length date == 10 && date >= from && date <= to


rangeText : Day -> Day -> String
rangeText from to =
    formatDay from ++ " – " ++ formatDay to


formatDay : Day -> String
formatDay day =
    String.fromInt day.day ++ " " ++ shortMonth day.month ++ " " ++ String.fromInt day.year


iso : Day -> String
iso day =
    String.fromInt day.year ++ "-" ++ pad2 day.month ++ "-" ++ pad2 day.day


addMonths : Int -> Day -> Day
addMonths delta day =
    let
        index =
            day.year * 12 + (day.month - 1) + delta

        year =
            index // 12

        month =
            modBy 12 index + 1
    in
    { year = year
    , month = month
    , day = min day.day (daysInMonth year month)
    }


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        1 ->
            31

        2 ->
            if isLeap year then
                29

            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        12 ->
            31

        _ ->
            30


isLeap : Int -> Bool
isLeap year =
    modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0)


pad2 : Int -> String
pad2 n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


capitalize : String -> String
capitalize raw =
    String.toUpper (String.left 1 raw) ++ String.dropLeft 1 raw


monthName : Int -> String
monthName month =
    case month of
        1 ->
            "janvier"

        2 ->
            "février"

        3 ->
            "mars"

        4 ->
            "avril"

        5 ->
            "mai"

        6 ->
            "juin"

        7 ->
            "juillet"

        8 ->
            "août"

        9 ->
            "septembre"

        10 ->
            "octobre"

        11 ->
            "novembre"

        12 ->
            "décembre"

        _ ->
            ""


shortMonth : Int -> String
shortMonth month =
    case month of
        1 ->
            "jan."

        2 ->
            "fév."

        3 ->
            "mar."

        4 ->
            "avr."

        5 ->
            "mai"

        6 ->
            "juin"

        7 ->
            "juil."

        8 ->
            "août"

        9 ->
            "sep."

        10 ->
            "oct."

        11 ->
            "nov."

        12 ->
            "déc."

        _ ->
            ""


shownAircraft : List AircraftTotal -> List AircraftTotal
shownAircraft rows =
    if List.length rows > 10 then
        List.filter (\row -> row.minutes >= fiveHours) rows

    else
        rows


fiveHours : Int
fiveHours =
    5 * 60


insertAircraft : LocalLog -> Dict String AircraftTotal -> Dict String AircraftTotal
insertAircraft log dict =
    let
        raw =
            String.trim (aircraftType log.entry)

        key =
            String.toUpper raw

        label =
            if raw == "" then
                "Type non renseigné"

            else
                raw

        current =
            Dict.get key dict
                |> Maybe.withDefault
                    { aircraftType = label
                    , flights = 0
                    , minutes = 0
                    }
    in
    Dict.insert key
        { current
            | flights = current.flights + 1
            , minutes = current.minutes + blockMinutes log.entry
        }
        dict


blockMinutes : LogEntry -> Int
blockMinutes entry =
    case entry of
        PlaneDgacEntry log ->
            planeLogMinutes log

        HelicopterDgacEntry log ->
            planeLogMinutes log

        GliderDgacEntry log ->
            log.dual_minutes + log.pilot_in_command_minutes

        PlaneFcl1080Entry log ->
            log.total_time_flight

        UlmFfplumEntry log ->
            log.dual_minutes + log.pilot_in_command_minutes

        BalloonDgacEntry log ->
            log.dual_minutes + log.pilot_in_command_minutes

        ParachuteCloudyEntry log ->
            log.falling_time


planeLogMinutes : { a | crew_day_minutes : Int, crew_night_minutes : Int, single_engine_day_dual_minutes : Int, single_engine_day_captain_minutes : Int, single_engine_night_dual_minutes : Int, single_engine_night_captain_minutes : Int, multi_engine_day_dual_minutes : Int, multi_engine_day_captain_minutes : Int, multi_engine_day_co_pilot_minutes : Int, multi_engine_night_dual_minutes : Int, multi_engine_night_captain_minutes : Int, multi_engine_night_co_pilot_minutes : Int } -> Int
planeLogMinutes log =
    log.crew_day_minutes
        + log.crew_night_minutes
        + log.single_engine_day_dual_minutes
        + log.single_engine_day_captain_minutes
        + log.single_engine_night_dual_minutes
        + log.single_engine_night_captain_minutes
        + log.multi_engine_day_dual_minutes
        + log.multi_engine_day_captain_minutes
        + log.multi_engine_day_co_pilot_minutes
        + log.multi_engine_night_dual_minutes
        + log.multi_engine_night_captain_minutes
        + log.multi_engine_night_co_pilot_minutes


takeoffCount : LogEntry -> Int
takeoffCount entry =
    case entry of
        PlaneDgacEntry log ->
            log.takeoff_count

        HelicopterDgacEntry log ->
            log.takeoff_count

        GliderDgacEntry log ->
            log.takeoff_count

        PlaneFcl1080Entry log ->
            log.take_off_day + log.take_off_night

        UlmFfplumEntry log ->
            log.takeoff_count

        BalloonDgacEntry log ->
            log.takeoff_count

        ParachuteCloudyEntry _ ->
            0


landingCount : LogEntry -> Int
landingCount entry =
    case entry of
        PlaneDgacEntry log ->
            log.landing_count

        HelicopterDgacEntry log ->
            log.landing_count

        GliderDgacEntry log ->
            log.landing_count

        PlaneFcl1080Entry log ->
            log.landing_day + log.landing_night

        UlmFfplumEntry log ->
            log.landing_count

        BalloonDgacEntry log ->
            log.landing_count

        ParachuteCloudyEntry _ ->
            0


aircraftType : LogEntry -> String
aircraftType entry =
    case entry of
        PlaneDgacEntry log ->
            log.aircraft_type

        HelicopterDgacEntry log ->
            log.aircraft_type

        GliderDgacEntry log ->
            log.aircraft_type

        PlaneFcl1080Entry log ->
            log.aircraft_type

        UlmFfplumEntry log ->
            log.aircraft_type

        BalloonDgacEntry log ->
            log.ballon_type

        ParachuteCloudyEntry log ->
            log.aircraft


keep : List ( String, String ) -> List ( String, String )
keep =
    List.filter (\( _, value ) -> value /= "")


countLabel : Int -> String
countLabel n =
    if n == 0 then
        ""

    else
        String.fromInt n
