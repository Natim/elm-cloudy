module TimeFormat exposing (applyIsoDate, formatDdMmYyyy, minutesFromHhmm, minutesToHhmm, parseDdMmYyyy)

import Char


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


formatDdMmYyyy : String -> String
formatDdMmYyyy raw =
    let
        iso =
            String.left 10 raw

        year =
            String.slice 0 4 iso

        month =
            String.slice 5 7 iso

        day =
            String.slice 8 10 iso
    in
    if String.length iso == 10 && String.slice 4 5 iso == "-" && String.slice 7 8 iso == "-" && String.all Char.isDigit (year ++ month ++ day) then
        day ++ "/" ++ month ++ "/" ++ year

    else
        ""


parseDdMmYyyy : String -> Maybe String
parseDdMmYyyy raw =
    case String.split "/" (String.trim raw) of
        [ dayRaw, monthRaw, yearRaw ] ->
            case ( String.toInt dayRaw, String.toInt monthRaw, String.toInt yearRaw ) of
                ( Just day, Just month, Just year ) ->
                    if day >= 1 && day <= 31 && month >= 1 && month <= 12 && year >= 1900 && String.length yearRaw == 4 then
                        Just (String.fromInt year ++ "-" ++ pad2 month ++ "-" ++ pad2 day)

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


applyIsoDate : String -> String -> String
applyIsoDate isoDay previous =
    if String.length isoDay == 10 then
        case String.split "T" previous of
            _ :: time :: _ ->
                isoDay ++ "T" ++ time

            _ ->
                isoDay ++ "T00:00:00"

    else
        previous


pad2 : Int -> String
pad2 n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


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
