module MediaUrl exposing (absoluteUrl)


absoluteUrl : String -> String -> String
absoluteUrl baseUrl url =
    if String.startsWith "https://" url || String.startsWith "http://" url then
        url

    else if String.startsWith "/" url then
        origin baseUrl ++ url

    else
        origin baseUrl ++ "/" ++ url


origin : String -> String
origin baseUrl =
    case String.split "/" baseUrl of
        scheme :: "" :: host :: _ ->
            scheme ++ "//" ++ host

        _ ->
            baseUrl
