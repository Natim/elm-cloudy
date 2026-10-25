module Stats exposing (AircraftTotal, Summary, summarize)

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Report as Report exposing (ReportEntry(..))
import Cloudy.Data.Report.BalloonDgac as BalloonReport
import Cloudy.Data.Report.GliderDgac as GliderReport
import Cloudy.Data.Report.ParachuteCloudy as ParachuteReport
import Cloudy.Data.Report.PlaneDgac as PlaneReport
import Cloudy.Data.Report.PlaneFcl1080 as FclReport
import Cloudy.Data.Report.UlmFfplum as UlmReport
import Dict exposing (Dict)
import LocalLog exposing (LocalLog)
import LogHelpers
import LogbookFormat
import TimeFormat


type alias Summary =
    { flights : Int
    , minutes : Maybe Int
    , takeoffs : Maybe Int
    , landings : Maybe Int
    , fromReport : Bool
    , details : List ( String, String )
    , byAircraft : List AircraftTotal
    }


type alias AircraftTotal =
    { registration : String
    , aircraftType : String
    , flights : Int
    , minutes : Int
    }


type alias Official =
    { minutes : Maybe Int
    , takeoffs : Maybe Int
    , landings : Maybe Int
    }


summarize : List LocalLog -> Maybe ReportEntry -> Summary
summarize logs report =
    let
        fromLogs =
            List.foldl addLog emptyTotals logs

        official =
            Maybe.andThen officialTotals report
    in
    { flights = fromLogs.flights
    , minutes = choose .minutes official fromLogs.minutes
    , takeoffs = choose .takeoffs official fromLogs.takeoffs
    , landings = choose .landings official fromLogs.landings
    , fromReport = official /= Nothing
    , details = Maybe.map reportDetails report |> Maybe.withDefault []
    , byAircraft = aircraftTotals logs
    }


choose : (Official -> Maybe Int) -> Maybe Official -> Int -> Maybe Int
choose read official fallback =
    case Maybe.andThen read official of
        Just value ->
            Just value

        Nothing ->
            Just fallback


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


officialTotals : ReportEntry -> Maybe Official
officialTotals report =
    case report of
        Report.PlaneDgacReport totals ->
            Just (planeOfficial totals)

        Report.HelicopterDgacReport totals ->
            Just (planeOfficial totals)

        Report.GliderDgacReport totals ->
            Just (gliderOfficial totals)

        Report.BalloonDgacReport totals ->
            Just (balloonOfficial totals)

        Report.PlaneFcl1080Report totals ->
            Just
                { minutes = parseReportTime totals.total_time_flight
                , takeoffs = Just (totals.take_off_day + totals.take_off_night)
                , landings = Just (totals.landing_day + totals.landing_night)
                }

        Report.UlmFfplumReport totals ->
            Just
                { minutes = sumTimes [ totals.dual_minutes, totals.pilot_in_command_minutes ]
                , takeoffs = Just totals.takeoff_count
                , landings = Just totals.landing_count
                }

        Report.ParachuteCloudyReport _ ->
            Nothing


planeOfficial : PlaneReport.Report -> Official
planeOfficial totals =
    { minutes = sumTimes (planeBlock totals)
    , takeoffs = Just totals.takeoff_count
    , landings = Just totals.landing_count
    }


gliderOfficial : GliderReport.Report -> Official
gliderOfficial totals =
    { minutes = sumTimes (gliderBlock totals)
    , takeoffs = Just totals.takeoff_count
    , landings = Just totals.landing_count
    }


balloonOfficial : BalloonReport.Report -> Official
balloonOfficial totals =
    { minutes = sumTimes (balloonBlock totals)
    , takeoffs = Just totals.takeoff_count
    , landings = Just totals.landing_count
    }


reportDetails : ReportEntry -> List ( String, String )
reportDetails report =
    case report of
        Report.PlaneDgacReport totals ->
            planeDetails totals

        Report.HelicopterDgacReport totals ->
            planeDetails totals

        Report.GliderDgacReport totals ->
            gliderDetails totals

        Report.BalloonDgacReport totals ->
            balloonDetails totals

        Report.PlaneFcl1080Report totals ->
            keep
                [ ( "Temps de vol", show totals.total_time_flight )
                , ( "Pilote seul", show totals.single_pilot_time )
                , ( "Multipilote", show totals.multi_pilot_time )
                , ( "Commandant de bord", show totals.pilot_time_in_cmd )
                , ( "Double commande", show totals.pilot_time_dual )
                , ( "Copilote", show totals.co_pilot_minutes )
                , ( "Nuit", show totals.op_time_night )
                , ( "IFR", show totals.op_time_ifr )
                , ( "Instruction", show totals.pilot_time_inst )
                , ( "Simulateur", show totals.synthetic_time )
                ]

        Report.UlmFfplumReport totals ->
            keep
                [ ( "Double commande", show totals.dual_minutes )
                , ( "Commandant de bord", show totals.pilot_in_command_minutes )
                ]

        Report.ParachuteCloudyReport totals ->
            [ ( "Sauts", String.fromInt totals.jump_count ) ]


planeDetails : PlaneReport.Report -> List ( String, String )
planeDetails totals =
    keep
        (List.map2 Tuple.pair planeLabels (List.map show (planeBlock totals))
            ++ [ ( "IFR", countLabel totals.ifr )
               , ( "Simulateur", show totals.simulator_minutes )
               , ( "Instrument", show totals.instrument_pilot_minutes )
               ]
        )


gliderDetails : GliderReport.Report -> List ( String, String )
gliderDetails totals =
    keep (List.map2 Tuple.pair planeLabels (List.map show (gliderBlock totals)))


balloonDetails : BalloonReport.Report -> List ( String, String )
balloonDetails totals =
    keep (List.map2 Tuple.pair planeLabels (List.map show (balloonBlock totals)))


planeLabels : List String
planeLabels =
    [ "Équipage jour"
    , "Équipage nuit"
    , "Double jour"
    , "CDB jour"
    , "Double nuit"
    , "CDB nuit"
    , "Multi double jour"
    , "Multi CDB jour"
    , "Multi copi jour"
    , "Multi double nuit"
    , "Multi CDB nuit"
    , "Multi copi nuit"
    ]


planeBlock : PlaneReport.Report -> List String
planeBlock totals =
    [ totals.crew_day_minutes
    , totals.crew_night_minutes
    , totals.single_engine_day_dual_minutes
    , totals.single_engine_day_captain_minutes
    , totals.single_engine_night_dual_minutes
    , totals.single_engine_night_captain_minutes
    , totals.multi_engine_day_dual_minutes
    , totals.multi_engine_day_captain_minutes
    , totals.multi_engine_day_co_pilot_minutes
    , totals.multi_engine_night_dual_minutes
    , totals.multi_engine_night_captain_minutes
    , totals.multi_engine_night_co_pilot_minutes
    ]


gliderBlock : GliderReport.Report -> List String
gliderBlock totals =
    [ totals.crew_day_minutes
    , totals.crew_night_minutes
    , totals.single_engine_day_dual_minutes
    , totals.single_engine_day_captain_minutes
    , totals.single_engine_night_dual_minutes
    , totals.single_engine_night_captain_minutes
    , totals.multi_engine_day_dual_minutes
    , totals.multi_engine_day_captain_minutes
    , totals.multi_engine_day_co_pilot_minutes
    , totals.multi_engine_night_dual_minutes
    , totals.multi_engine_night_captain_minutes
    , totals.multi_engine_night_co_pilot_minutes
    ]


balloonBlock : BalloonReport.Report -> List String
balloonBlock totals =
    [ totals.crew_day_minutes
    , totals.crew_night_minutes
    , totals.single_engine_day_dual_minutes
    , totals.single_engine_day_captain_minutes
    , totals.single_engine_night_dual_minutes
    , totals.single_engine_night_captain_minutes
    , totals.multi_engine_day_dual_minutes
    , totals.multi_engine_day_captain_minutes
    , totals.multi_engine_day_co_pilot_minutes
    , totals.multi_engine_night_dual_minutes
    , totals.multi_engine_night_captain_minutes
    , totals.multi_engine_night_co_pilot_minutes
    ]


aircraftTotals : List LocalLog -> List AircraftTotal
aircraftTotals logs =
    logs
        |> List.foldl insertAircraft Dict.empty
        |> Dict.values
        |> List.sortBy (\item -> ( -item.minutes, item.registration ))


insertAircraft : LocalLog -> Dict String AircraftTotal -> Dict String AircraftTotal
insertAircraft log dict =
    let
        key =
            registration log.entry

        current =
            Dict.get key dict
                |> Maybe.withDefault
                    { registration = key
                    , aircraftType = aircraftType log.entry
                    , flights = 0
                    , minutes = 0
                    }

        nextType =
            if current.aircraftType == "" then
                aircraftType log.entry

            else
                current.aircraftType
    in
    Dict.insert key
        { current
            | aircraftType = nextType
            , flights = current.flights + 1
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


registration : LogEntry -> String
registration entry =
    case String.trim (LogHelpers.registration entry) of
        "" ->
            "Sans immatriculation"

        value ->
            String.toUpper value


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


sumTimes : List String -> Maybe Int
sumTimes values =
    List.foldl
        (\raw acc ->
            Maybe.map2 (+) acc (parseReportTime raw)
        )
        (Just 0)
        values


parseReportTime : String -> Maybe Int
parseReportTime raw =
    case String.split ":" (String.trim raw) of
        [ hours, minutes, _ ] ->
            TimeFormat.minutesFromHhmm (hours ++ ":" ++ minutes)

        _ ->
            TimeFormat.minutesFromHhmm raw


show : String -> String
show =
    LogbookFormat.showReportTime


keep : List ( String, String ) -> List ( String, String )
keep =
    List.filter (\( _, value ) -> value /= "")


countLabel : Int -> String
countLabel n =
    if n == 0 then
        ""

    else
        String.fromInt n
