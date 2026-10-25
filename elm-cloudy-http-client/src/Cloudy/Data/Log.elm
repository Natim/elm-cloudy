module Cloudy.Data.Log exposing (LogEntry(..), decoder, decoderFromEntry, encodeCreate, encodePatch, encodeReplace, responseDecoder, responseDecoderFromEntry)

import Cloudy.Data.Log.BalloonDgac as BalloonDgac
import Cloudy.Data.Log.GliderDgac as GliderDgac
import Cloudy.Data.Log.ParachuteCloudy as ParachuteCloudy
import Cloudy.Data.Log.PlaneDgac as PlaneDgac
import Cloudy.Data.Log.PlaneFcl1080 as PlaneFcl1080
import Cloudy.Data.Log.UlmFfplum as UlmFfplum
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type LogEntry
    = PlaneDgacEntry PlaneDgac.Log
    | GliderDgacEntry GliderDgac.Log
    | PlaneFcl1080Entry PlaneFcl1080.Log
    | HelicopterDgacEntry PlaneDgac.Log
    | UlmFfplumEntry UlmFfplum.Log
    | BalloonDgacEntry BalloonDgac.Log
    | ParachuteCloudyEntry ParachuteCloudy.Log


responseDecoder : LogbookCategory -> Decoder LogEntry
responseDecoder category =
    Decode.field "log" (decoder category)


decoder : LogbookCategory -> Decoder LogEntry
decoder category =
    case category of
        PlaneDgac ->
            Decode.map PlaneDgacEntry PlaneDgac.decoder

        HelicopterDgac ->
            Decode.map HelicopterDgacEntry PlaneDgac.decoder

        GliderDgac ->
            Decode.map GliderDgacEntry GliderDgac.decoder

        PlaneFcl1080 ->
            Decode.map PlaneFcl1080Entry PlaneFcl1080.decoder

        UlmFfplum ->
            Decode.map UlmFfplumEntry UlmFfplum.decoder

        BalloonDgac ->
            Decode.map BalloonDgacEntry BalloonDgac.decoder

        ParachuteCloudy ->
            Decode.map ParachuteCloudyEntry ParachuteCloudy.decoder


encodeCreate : LogEntry -> Value
encodeCreate entry =
    case entry of
        PlaneDgacEntry log ->
            PlaneDgac.encodeCreate log

        HelicopterDgacEntry log ->
            PlaneDgac.encodeCreate log

        GliderDgacEntry log ->
            GliderDgac.encodeCreate log

        PlaneFcl1080Entry log ->
            PlaneFcl1080.encodeCreate log

        UlmFfplumEntry log ->
            UlmFfplum.encodeCreate log

        BalloonDgacEntry log ->
            BalloonDgac.encodeCreate log

        ParachuteCloudyEntry log ->
            ParachuteCloudy.encodeCreate log


encodeReplace : LogEntry -> Value
encodeReplace entry =
    case entry of
        PlaneDgacEntry log ->
            PlaneDgac.encodeReplace log

        HelicopterDgacEntry log ->
            PlaneDgac.encodeReplace log

        GliderDgacEntry log ->
            GliderDgac.encodeReplace log

        PlaneFcl1080Entry log ->
            PlaneFcl1080.encodeReplace log

        UlmFfplumEntry log ->
            UlmFfplum.encodeReplace log

        BalloonDgacEntry log ->
            BalloonDgac.encodeReplace log

        ParachuteCloudyEntry log ->
            ParachuteCloudy.encodeReplace log


encodePatch : LogbookCategory -> Value -> Value
encodePatch _ body =
    body


responseDecoderFromEntry : LogEntry -> Decoder LogEntry
responseDecoderFromEntry entry =
    Decode.field "log" (decoderFromEntry entry)


decoderFromEntry : LogEntry -> Decoder LogEntry
decoderFromEntry entry =
    case entry of
        PlaneDgacEntry _ ->
            decoder PlaneDgac

        HelicopterDgacEntry _ ->
            decoder HelicopterDgac

        GliderDgacEntry _ ->
            decoder GliderDgac

        PlaneFcl1080Entry _ ->
            decoder PlaneFcl1080

        UlmFfplumEntry _ ->
            decoder UlmFfplum

        BalloonDgacEntry _ ->
            decoder BalloonDgac

        ParachuteCloudyEntry _ ->
            decoder ParachuteCloudy
