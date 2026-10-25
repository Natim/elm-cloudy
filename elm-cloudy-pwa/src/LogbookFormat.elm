module LogbookFormat exposing (flightLabel, shortDate, showMinutes, showReportTime)

import TimeFormat


shortDate : String -> String
shortDate raw =
    let
        iso =
            String.left 10 raw

        day =
            String.slice 8 10 iso

        month =
            String.slice 5 7 iso
    in
    case String.toInt day of
        Just n ->
            String.fromInt n ++ " " ++ monthName month

        Nothing ->
            iso


flightLabel : String -> String -> String -> String -> String
flightLabel departure arrival nature observations =
    let
        dep =
            String.trim departure

        arr =
            String.trim arrival

        route =
            if dep /= "" && arr /= "" then
                dep ++ " - " ++ arr

            else if dep /= "" then
                dep

            else
                arr
    in
    if route /= "" then
        route

    else if String.trim nature /= "" then
        nature

    else
        observations


showMinutes : Int -> String
showMinutes total =
    if total == 0 then
        ""

    else
        TimeFormat.minutesToHhmm total


showReportTime : String -> String
showReportTime raw =
    let
        trimmed =
            String.trim raw
    in
    if trimmed == "" || trimmed == "0" || trimmed == "0:00" || trimmed == "00:00" || trimmed == "0:00:00" then
        ""

    else
        trimmed


monthName : String -> String
monthName month =
    case month of
        "01" ->
            "jan"

        "02" ->
            "fév"

        "03" ->
            "mar"

        "04" ->
            "avr"

        "05" ->
            "mai"

        "06" ->
            "juin"

        "07" ->
            "juil"

        "08" ->
            "août"

        "09" ->
            "sep"

        "10" ->
            "oct"

        "11" ->
            "nov"

        "12" ->
            "déc"

        _ ->
            month
