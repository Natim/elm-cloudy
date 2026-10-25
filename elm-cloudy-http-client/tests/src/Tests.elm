module Tests exposing (tests)

import Cloudy.Data.ApiErrorBody as ApiErrorBody
import Cloudy.Data.AttachedFile as AttachedFile
import Cloudy.Data.Log as Log exposing (LogEntry(..))
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.LogListPage as LogListPage
import Cloudy.Data.LogbookCategory as LogbookCategory
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Cloudy.Data"
        [ test "decodes logbook category" <|
            \_ ->
                LogbookCategory.fromString "plane_dgac"
                    |> Expect.equal (Just LogbookCategory.PlaneDgac)
        , test "decodes attached file upload response" <|
            \_ ->
                let
                    json =
                        """{"id":7,"url":"/media/upload/scan.jpg"}"""
                in
                Decode.decodeString AttachedFile.decoder json
                    |> Result.map (\file -> ( file.id, file.url ))
                    |> Expect.equal (Ok ( 7, "/media/upload/scan.jpg" ))
        , test "decodes API error body" <|
            \_ ->
                let
                    json =
                        """{"detail":"Invalid log","code":"invalid","errors":{"flight_date":[{"message":"required","code":"required"}]}}"""
                in
                Decode.decodeString ApiErrorBody.decoder json
                    |> Result.map .detail
                    |> Expect.equal (Ok "Invalid log")
        , test "decodes empty log list page" <|
            \_ ->
                let
                    json =
                        """{"items":[],"next_page_token":null}"""
                in
                Decode.decodeString (LogListPage.decoder LogbookCategory.PlaneDgac) json
                    |> Result.map .nextPageToken
                    |> Expect.equal (Ok Nothing)
        , test "decodes a log wrapped in a log field" <|
            \_ ->
                let
                    json =
                        """{"log":{"logbook_id":1,"id":2,"visible":true,"deleted":false,"flight_date":"2011-07-04T12:00:00","updated_at":"2011-07-04T12:00:00","created_at":"2011-07-04T12:00:00","optional_filter":"","aircraft_type":"T1","aircraft_registration":"M1","function_on_board":"EP","flight_nature":"N1","crew_day_minutes":0,"crew_night_minutes":1,"single_engine_day_dual_minutes":0,"single_engine_day_captain_minutes":0,"single_engine_night_dual_minutes":0,"single_engine_night_captain_minutes":0,"multi_engine_day_dual_minutes":0,"multi_engine_day_captain_minutes":0,"multi_engine_day_co_pilot_minutes":0,"multi_engine_night_dual_minutes":0,"multi_engine_night_captain_minutes":0,"multi_engine_night_co_pilot_minutes":0,"instrument_day_dual_minutes":0,"instrument_pilot_minutes":0,"simulator_minutes":0,"ifr":0,"observations":"","comments":"","takeoff_count":0,"landing_count":0,"departure_airport":"","arrival_airport":"","passengers":[],"files":[]}}"""
                in
                Decode.decodeString (Log.responseDecoder LogbookCategory.PlaneDgac) json
                    |> Result.map
                        (\entry ->
                            case entry of
                                Log.PlaneDgacEntry log ->
                                    log.flight_nature

                                _ ->
                                    ""
                        )
                    |> Expect.equal (Ok "N1")
        , test "decodes a log returned directly by the API" <|
            \_ ->
                let
                    json =
                        """{"logbook_id":1,"id":2,"visible":true,"deleted":false,"flight_date":"2011-07-04T12:00:00","updated_at":"2011-07-04T12:00:00","created_at":"2011-07-04T12:00:00","optional_filter":"","aircraft_type":"T1","aircraft_registration":"M1","function_on_board":"EP","flight_nature":"N1","crew_day_minutes":0,"crew_night_minutes":1,"single_engine_day_dual_minutes":0,"single_engine_day_captain_minutes":0,"single_engine_night_dual_minutes":0,"single_engine_night_captain_minutes":0,"multi_engine_day_dual_minutes":0,"multi_engine_day_captain_minutes":0,"multi_engine_day_co_pilot_minutes":0,"multi_engine_night_dual_minutes":0,"multi_engine_night_captain_minutes":0,"multi_engine_night_co_pilot_minutes":0,"instrument_day_dual_minutes":0,"instrument_pilot_minutes":0,"simulator_minutes":0,"ifr":0,"observations":"","comments":"","takeoff_count":0,"landing_count":0,"departure_airport":"","arrival_airport":"","passengers":[],"files":[]}"""
                in
                Decode.decodeString (Log.responseDecoder LogbookCategory.PlaneDgac) json
                    |> Result.map
                        (\entry ->
                            case entry of
                                Log.PlaneDgacEntry log ->
                                    log.id

                                _ ->
                                    0
                        )
                    |> Expect.equal (Ok 2)
        , test "decodes plane dgac log snippet" <|
            \_ ->
                let
                    json =
                        """{"logbook_id":1,"id":2,"visible":true,"deleted":false,"flight_date":"2011-07-04T12:00:00","updated_at":"2011-07-04T12:00:00","created_at":"2011-07-04T12:00:00","optional_filter":"","aircraft_type":"T1","aircraft_registration":"M1","function_on_board":"EP","flight_nature":"N1","crew_day_minutes":0,"crew_night_minutes":1,"single_engine_day_dual_minutes":0,"single_engine_day_captain_minutes":0,"single_engine_night_dual_minutes":0,"single_engine_night_captain_minutes":0,"multi_engine_day_dual_minutes":0,"multi_engine_day_captain_minutes":0,"multi_engine_day_co_pilot_minutes":0,"multi_engine_night_dual_minutes":0,"multi_engine_night_captain_minutes":0,"multi_engine_night_co_pilot_minutes":0,"instrument_day_dual_minutes":0,"instrument_pilot_minutes":0,"simulator_minutes":0,"ifr":0,"observations":"","comments":"","takeoff_count":0,"landing_count":0,"departure_airport":"","arrival_airport":"","passengers":[],"files":[]}"""
                in
                Decode.decodeString PlaneDgac.decoder json
                    |> Result.map .flight_nature
                    |> Expect.equal (Ok "N1")
        , test "decodes FCL single pilot time in minutes" <|
            \_ ->
                let
                    json =
                        """{"logbook_id":1,"id":2,"visible":true,"deleted":false,"flight_date":"2011-08-01T08:00:00","updated_at":"2011-08-01T08:00:00","created_at":"2011-08-01T08:00:00","optional_filter":"","departure_place":"LFPG","departure_time":"08:00","arrival_place":"LFPO","arrival_time":"09:00","aircraft_type":"C172","aircraft_registration":"F-GABC","single_pilot_se":true,"single_pilot_me":false,"single_pilot_time":47,"multi_pilot_time":0,"total_time_flight":47,"pilot_in_command_name":"Ada Pilot","take_off_day":1,"take_off_night":0,"landing_day":1,"landing_night":0,"op_time_night":0,"op_time_ifr":0,"pilot_time_in_cmd":47,"co_pilot_minutes":0,"pilot_time_dual":0,"pilot_time_inst":0,"synthetic_date":"","synthetic_type":"","synthetic_time":0,"observations":"","comments":"","passengers":[],"files":[]}"""
                in
                Decode.decodeString PlaneFcl1080.decoder json
                    |> Result.map (\log -> ( log.single_pilot_se, log.single_pilot_me, log.single_pilot_time ))
                    |> Expect.equal (Ok ( True, False, 47 ))
        , test "decodes FCL logs that omit synthetic_date" <|
            \_ ->
                let
                    json =
                        """{"logbook_id":5389,"id":444797,"visible":true,"deleted":false,"flight_date":"2026-10-22T14:12:06.511667+00:00","updated_at":"2026-10-22T14:12:06.511883+00:00","created_at":"2026-10-22T14:12:06.511667+00:00","optional_filter":"","departure_place":"LFRN","departure_time":"07:45","arrival_place":"LFRN","arrival_time":"08:32","aircraft_type":"PA28","aircraft_registration":"F-GHYZ","single_pilot_se":true,"single_pilot_me":false,"single_pilot_time":47,"multi_pilot_time":0,"total_time_flight":47,"pilot_in_command_name":"SELF","take_off_day":5,"take_off_night":0,"landing_day":5,"landing_night":0,"op_time_night":0,"op_time_ifr":0,"pilot_time_in_cmd":0,"co_pilot_minutes":0,"pilot_time_dual":0,"pilot_time_inst":47,"synthetic_type":"","synthetic_time":0,"observations":"","comments":"","passengers":["Anasthasia Beaujot"],"files":[]}"""
                in
                Decode.decodeString (LogListPage.decoder LogbookCategory.PlaneFcl1080) ("{\"items\":[" ++ json ++ "]}")
                    |> Result.map
                        (\page ->
                            case List.head page.items of
                                Just (PlaneFcl1080Entry log) ->
                                    ( log.flight_date, log.synthetic_date )

                                _ ->
                                    ( "", "missing" )
                        )
                    |> Expect.equal (Ok ( "2026-10-22T14:12:06.511667+00:00", "" ))
        ]
