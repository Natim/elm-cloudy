module Cloudy.Data.Report exposing (ReportEntry(..), decoder, encodeReplace)

import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Cloudy.Data.Report.BalloonDgac as BalloonDgac
import Cloudy.Data.Report.GliderDgac as GliderDgac
import Cloudy.Data.Report.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Report.PlaneDgac as PlaneDgac
import Cloudy.Data.Report.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Report.UlmFfplum as UlmFfplum
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type ReportEntry
    = PlaneDgacReport PlaneDgac.Report
    | GliderDgacReport GliderDgac.Report
    | PlaneFcl1080Report PlaneFcl1080.Report
    | HelicopterDgacReport PlaneDgac.Report
    | UlmFfplumReport UlmFfplum.Report
    | BalloonDgacReport BalloonDgac.Report
    | ParachuteCloudyReport ParachuteCloudy.Report


decoder : LogbookCategory -> Decoder ReportEntry
decoder category =
    case category of
        PlaneDgac ->
            Decode.map PlaneDgacReport PlaneDgac.decoder

        HelicopterDgac ->
            Decode.map HelicopterDgacReport PlaneDgac.decoder

        GliderDgac ->
            Decode.map GliderDgacReport GliderDgac.decoder

        PlaneFcl1080 ->
            Decode.map PlaneFcl1080Report PlaneFcl1080.decoder

        UlmFfplum ->
            Decode.map UlmFfplumReport UlmFfplum.decoder

        BalloonDgac ->
            Decode.map BalloonDgacReport BalloonDgac.decoder

        ParachuteCloudy ->
            Decode.map ParachuteCloudyReport ParachuteCloudy.decoder


encodeReplace : ReportEntry -> Value
encodeReplace entry =
    case entry of
        PlaneDgacReport report ->
            PlaneDgac.encodeReplace report

        HelicopterDgacReport report ->
            PlaneDgac.encodeReplace report

        GliderDgacReport report ->
            GliderDgac.encodeReplace report

        PlaneFcl1080Report report ->
            PlaneFcl1080.encodeReplace report

        UlmFfplumReport report ->
            UlmFfplum.encodeReplace report

        BalloonDgacReport report ->
            BalloonDgac.encodeReplace report

        ParachuteCloudyReport report ->
            ParachuteCloudy.encodeReplace report
