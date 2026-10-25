module TimeFormat exposing (minutesFromHhmm, minutesToHhmm)


minutesFromHhmm : String -> Maybe Int
minutesFromHhmm raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        Just 0

    else
        case String.split ":" trimmed of
            [ h, m ] ->
                case ( String.toInt h, String.toInt m ) of
                    ( Just hours, Just mins ) ->
                        if mins >= 0 && mins < 60 then
                            Just (hours * 60 + mins)

                        else
                            Nothing

                    _ ->
                        Nothing

            _ ->
                case String.toInt trimmed of
                    Just n ->
                        Just n

                    Nothing ->
                        Nothing


minutesToHhmm : Int -> String
minutesToHhmm total =
    let
        hours =
            total // 60

        mins =
            modBy 60 total
    in
    String.fromInt hours
        ++ ":"
        ++ (if mins < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt mins
