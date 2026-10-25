module Tests exposing (tests)

import Cloudy.Data.ApiErrorBody as ApiErrorBody
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
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
        ]
