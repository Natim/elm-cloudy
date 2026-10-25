module Cloudy.Reports exposing (get, replace)

import Cloudy.Client exposing (Client)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory)
import Cloudy.Data.Report as Report exposing (ReportEntry(..))
import Cloudy.Request as Request exposing (Request(..))


get : Client -> LogbookCategory -> Int -> Request ReportEntry
get client category logbookId =
    Request.get client [ "logbooks", String.fromInt logbookId, "reports" ] [] (Report.decoder category)


replace : Client -> Int -> ReportEntry -> Request ReportEntry
replace client logbookId entry =
    let
        category =
            reportCategory entry
    in
    Request.put client [ "logbooks", String.fromInt logbookId, "reports" ] (Report.encodeReplace entry) (Report.decoder category)


reportCategory : ReportEntry -> LogbookCategory
reportCategory entry =
    case entry of
        Report.PlaneDgacReport _ ->
            LogbookCategory.PlaneDgac

        Report.HelicopterDgacReport _ ->
            LogbookCategory.HelicopterDgac

        Report.GliderDgacReport _ ->
            LogbookCategory.GliderDgac

        Report.PlaneFcl1080Report _ ->
            LogbookCategory.PlaneFcl1080

        Report.UlmFfplumReport _ ->
            LogbookCategory.UlmFfplum

        Report.BalloonDgacReport _ ->
            LogbookCategory.BalloonDgac

        Report.ParachuteCloudyReport _ ->
            LogbookCategory.ParachuteCloudy
