module Stats exposing (AircraftTotal, Summary, shownAircraft, summarize)

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Log.BalloonDgac as BalloonDgac
import Cloudy.Data.Log.GliderDgac as GliderDgac
import Cloudy.Data.Log.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Log.UlmFfplum as UlmFfplum
import Dict exposing (Dict)
import LocalLog exposing (LocalLog)
import LogbookFormat


type alias Summary =
    { flights : Int
    , minutes : Maybe Int
    , takeoffs : Maybe Int
    , landings : Maybe Int
    , details : List ( String, String )
    , byAircraft : List AircraftTotal
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
    in
    { flights = fromLogs.flights
    , minutes = Just fromLogs.minutes
    , takeoffs = Just fromLogs.takeoffs
    , landings = Just fromLogs.landings
    , details = flightDetails logs
    , byAircraft = aircraftTotals logs
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
        [ ( "Temps de vol", LogbookFormat.showMinutes (summedInt fclMinutes .total_time_flight logs) )
        , ( "Pilote seul", LogbookFormat.showMinutes (summedInt fclMinutes .single_pilot_time logs) )
        , ( "Multipilote", LogbookFormat.showMinutes (summedInt fclMinutes .multi_pilot_time logs) )
        , ( "Commandant de bord", LogbookFormat.showMinutes (summedInt fclMinutes .pilot_time_in_cmd logs) )
        , ( "Double commande", LogbookFormat.showMinutes (summedInt fclMinutes .pilot_time_dual logs) )
        , ( "Copilote", LogbookFormat.showMinutes (summedInt fclMinutes .co_pilot_minutes logs) )
        , ( "Nuit", LogbookFormat.showMinutes (summedInt fclMinutes .op_time_night logs) )
        , ( "IFR", LogbookFormat.showMinutes (summedInt fclMinutes .op_time_ifr logs) )
        , ( "Instruction", LogbookFormat.showMinutes (summedInt fclMinutes .pilot_time_inst logs) )
        , ( "Simulateur", LogbookFormat.showMinutes (summedInt fclMinutes .synthetic_time logs) )
        ]


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
