module Tests exposing (tests)

import Cloudy.Data.ApiErrorBody as ApiErrorBody
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
        ]
