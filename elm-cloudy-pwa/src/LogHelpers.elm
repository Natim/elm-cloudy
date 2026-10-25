module LogHelpers exposing (emptyEntry, flightDate, logbookId, registration, serverId, setFlightDate, title)

import Cloudy.Data.Log exposing (LogEntry(..))
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.LogbookCategory exposing (LogbookCategory(..))


serverId : LogEntry -> Int
serverId entry =
    case entry of
        PlaneDgacEntry log ->
            log.id

        HelicopterDgacEntry log ->
            log.id

        GliderDgacEntry log ->
            log.id

        PlaneFcl1080Entry log ->
            log.id

        UlmFfplumEntry log ->
            log.id

        BalloonDgacEntry log ->
            log.id

        ParachuteCloudyEntry log ->
            log.id


logbookId : LogEntry -> Int
logbookId entry =
    case entry of
        PlaneDgacEntry log ->
            log.logbook_id

        HelicopterDgacEntry log ->
            log.logbook_id

        GliderDgacEntry log ->
            log.logbook_id

        PlaneFcl1080Entry log ->
            log.logbook_id

        UlmFfplumEntry log ->
            log.logbook_id

        BalloonDgacEntry log ->
            log.logbook_id

        ParachuteCloudyEntry log ->
            log.logbook_id


flightDate : LogEntry -> String
flightDate entry =
    case entry of
        PlaneDgacEntry log ->
            log.flight_date

        HelicopterDgacEntry log ->
            log.flight_date

        GliderDgacEntry log ->
            log.flight_date

        PlaneFcl1080Entry log ->
            log.flight_date

        UlmFfplumEntry log ->
            log.flight_date

        BalloonDgacEntry log ->
            log.flight_date

        ParachuteCloudyEntry log ->
            log.flight_date


registration : LogEntry -> String
registration entry =
    case entry of
        PlaneDgacEntry log ->
            log.aircraft_registration

        HelicopterDgacEntry log ->
            log.aircraft_registration

        GliderDgacEntry log ->
            log.aircraft_registration

        PlaneFcl1080Entry log ->
            log.aircraft_registration

        UlmFfplumEntry log ->
            log.aircraft_registration

        BalloonDgacEntry log ->
            log.ballon_imma

        ParachuteCloudyEntry log ->
            log.aircraft


setFlightDate : String -> LogEntry -> LogEntry
setFlightDate value entry =
    case entry of
        PlaneDgacEntry log ->
            PlaneDgacEntry { log | flight_date = value }

        HelicopterDgacEntry log ->
            HelicopterDgacEntry { log | flight_date = value }

        GliderDgacEntry log ->
            GliderDgacEntry { log | flight_date = value }

        PlaneFcl1080Entry log ->
            PlaneFcl1080Entry { log | flight_date = value }

        UlmFfplumEntry log ->
            UlmFfplumEntry { log | flight_date = value }

        BalloonDgacEntry log ->
            BalloonDgacEntry { log | flight_date = value }

        ParachuteCloudyEntry log ->
            ParachuteCloudyEntry { log | flight_date = value }


title : LogEntry -> String
title entry =
    let
        reg =
            registration entry

        date =
            String.left 10 (flightDate entry)
    in
    if String.isEmpty reg then
        "Vol " ++ date

    else
        reg ++ " · " ++ date


emptyEntry : LogbookCategory -> Int -> String -> LogEntry
emptyEntry category bookId flightDateIso =
    case category of
        PlaneDgac ->
            PlaneDgacEntry (emptyPlaneDgac bookId flightDateIso)

        HelicopterDgac ->
            HelicopterDgacEntry (emptyPlaneDgac bookId flightDateIso)

        GliderDgac ->
            GliderDgacEntry
                { logbook_id = bookId
                , id = 0
                , visible = True
                , deleted = False
                , flight_date = flightDateIso
                , updated_at = flightDateIso
                , created_at = flightDateIso
                , optional_filter = ""
                , aircraft_type = ""
                , aircraft_registration = ""
                , dual_minutes = 0
                , pilot_in_command_minutes = 0
                , instructor_minutes = 0
                , function_on_board = "Pilote"
                , flight_nature = "L"
                , lmode = "R"
                , observations = ""
                , comments = ""
                , takeoff_count = 0
                , landing_count = 0
                , departure_airport = ""
                , arrival_airport = ""
                , passengers = []
                , files = []
                }

        PlaneFcl1080 ->
            PlaneFcl1080Entry
                { logbook_id = bookId
                , id = 0
                , visible = True
                , deleted = False
                , flight_date = flightDateIso
                , updated_at = flightDateIso
                , created_at = flightDateIso
                , optional_filter = ""
                , departure_place = ""
                , departure_time = "08:00"
                , arrival_place = ""
                , arrival_time = "09:00"
                , aircraft_type = ""
                , aircraft_registration = ""
                , single_pilot_se = 0
                , single_pilot_me = 0
                , multi_pilot_time = 0
                , total_time_flight = 0
                , pilot_in_command_name = ""
                , take_off_day = 0
                , take_off_night = 0
                , landing_day = 0
                , landing_night = 0
                , op_time_night = 0
                , op_time_ifr = 0
                , pilot_time_in_cmd = 0
                , co_pilot_minutes = 0
                , pilot_time_dual = 0
                , pilot_time_inst = 0
                , synthetic_date = flightDateIso
                , synthetic_type = ""
                , synthetic_time = 0
                , observations = ""
                , comments = ""
                , passengers = []
                , files = []
                }

        UlmFfplum ->
            UlmFfplumEntry
                { logbook_id = bookId
                , id = 0
                , visible = True
                , deleted = False
                , flight_date = flightDateIso
                , updated_at = flightDateIso
                , created_at = flightDateIso
                , optional_filter = ""
                , aircraft_type = ""
                , aircraft_registration = ""
                , dual_minutes = 0
                , pilot_in_command_minutes = 0
                , observations = ""
                , comments = ""
                , takeoff_count = 0
                , landing_count = 0
                , departure_airport = ""
                , arrival_airport = ""
                , passengers = []
                , files = []
                }

        BalloonDgac ->
            BalloonDgacEntry
                { logbook_id = bookId
                , id = 0
                , visible = True
                , deleted = False
                , flight_date = flightDateIso
                , updated_at = flightDateIso
                , created_at = flightDateIso
                , optional_filter = ""
                , ballon_type = ""
                , ballon_imma = ""
                , dual_minutes = 0
                , pilot_in_command_minutes = 0
                , observations = ""
                , comments = ""
                , takeoff_count = 0
                , landing_count = 0
                , departure_airport = ""
                , arrival_airport = ""
                , passengers = []
                , files = []
                }

        ParachuteCloudy ->
            ParachuteCloudyEntry
                { place = ""
                , aircraft = ""
                , altitude = 0
                , altitude_open = 0
                , canopy = ""
                , bag = ""
                , perf_exercise = ""
                , freefall_training = ""
                , canopy_piloting = ""
                , falling_time = 0
                , logbook_id = bookId
                , id = 0
                , visible = True
                , deleted = False
                , flight_date = flightDateIso
                , updated_at = flightDateIso
                , created_at = flightDateIso
                , optional_filter = ""
                , comments = ""
                , files = []
                }


emptyPlaneDgac : Int -> String -> PlaneDgac.Log
emptyPlaneDgac bookId flightDateIso =
    { logbook_id = bookId
    , id = 0
    , visible = True
    , deleted = False
    , flight_date = flightDateIso
    , updated_at = flightDateIso
    , created_at = flightDateIso
    , optional_filter = ""
    , aircraft_type = ""
    , aircraft_registration = ""
    , function_on_board = "Pilote"
    , flight_nature = ""
    , crew_day_minutes = 0
    , crew_night_minutes = 0
    , single_engine_day_dual_minutes = 0
    , single_engine_day_captain_minutes = 0
    , single_engine_night_dual_minutes = 0
    , single_engine_night_captain_minutes = 0
    , multi_engine_day_dual_minutes = 0
    , multi_engine_day_captain_minutes = 0
    , multi_engine_day_co_pilot_minutes = 0
    , multi_engine_night_dual_minutes = 0
    , multi_engine_night_captain_minutes = 0
    , multi_engine_night_co_pilot_minutes = 0
    , instrument_day_dual_minutes = 0
    , instrument_pilot_minutes = 0
    , simulator_minutes = 0
    , ifr = 0
    , observations = ""
    , comments = ""
    , takeoff_count = 0
    , landing_count = 0
    , departure_airport = ""
    , arrival_airport = ""
    , passengers = []
    , files = []
    }
