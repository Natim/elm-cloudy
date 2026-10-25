module Cloudy.Data.LogbookCategory exposing (LogbookCategory(..), decoder, fromString, logSchemaModule, toString)

import Json.Decode as Decode exposing (Decoder)


type LogbookCategory
    = PlaneDgac
    | GliderDgac
    | PlaneFcl1080
    | HelicopterDgac
    | UlmFfplum
    | BalloonDgac
    | ParachuteCloudy


fromString : String -> Maybe LogbookCategory
fromString raw =
    case raw of
        "plane_dgac" ->
            Just PlaneDgac

        "glider_dgac" ->
            Just GliderDgac

        "plane_fcl1080" ->
            Just PlaneFcl1080

        "helicopter_dgac" ->
            Just HelicopterDgac

        "ulm_ffplum" ->
            Just UlmFfplum

        "balloon_dgac" ->
            Just BalloonDgac

        "parachute_cloudy" ->
            Just ParachuteCloudy

        _ ->
            Nothing


toString : LogbookCategory -> String
toString category =
    case category of
        PlaneDgac ->
            "plane_dgac"

        GliderDgac ->
            "glider_dgac"

        PlaneFcl1080 ->
            "plane_fcl1080"

        HelicopterDgac ->
            "helicopter_dgac"

        UlmFfplum ->
            "ulm_ffplum"

        BalloonDgac ->
            "balloon_dgac"

        ParachuteCloudy ->
            "parachute_cloudy"


decoder : Decoder LogbookCategory
decoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case fromString raw of
                    Just category ->
                        Decode.succeed category

                    Nothing ->
                        Decode.fail ("Unknown logbook category: " ++ raw)
            )


logSchemaModule : LogbookCategory -> String
logSchemaModule category =
    case category of
        PlaneDgac ->
            "PlaneDgac"

        HelicopterDgac ->
            "PlaneDgac"

        GliderDgac ->
            "GliderDgac"

        PlaneFcl1080 ->
            "PlaneFcl1080"

        UlmFfplum ->
            "UlmFfplum"

        BalloonDgac ->
            "BalloonDgac"

        ParachuteCloudy ->
            "ParachuteCloudy"
