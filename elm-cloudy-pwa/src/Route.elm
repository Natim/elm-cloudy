module Route exposing (Route(..), fromUrl, toPath)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string, top)


type Route
    = Login
    | Logbooks
    | Logs Int
    | EditorNew Int
    | EditorEdit Int String


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Login (s "login")
        , Parser.map Logbooks (s "logbooks")
        , Parser.map EditorNew (s "logbooks" </> Parser.int </> s "new")
        , Parser.map EditorEdit (s "logbooks" </> Parser.int </> s "edit" </> string)
        , Parser.map Logs (s "logbooks" </> Parser.int)
        ]


fromUrl : Url -> Route
fromUrl url =
    let
        path =
            normalizePath url.path
    in
    case Parser.parse (Parser.top </> parser) { url | path = path } of
        Just route ->
            route

        Nothing ->
            Login


normalizePath : String -> String
normalizePath path =
    if String.startsWith "/elm-cloudy/" path then
        String.dropLeft (String.length "/elm-cloudy") path

    else if path == "/elm-cloudy" || path == "/elm-cloudy/" then
        "/"

    else
        path


toPath : Route -> String
toPath route =
    case route of
        Login ->
            "/login"

        Logbooks ->
            "/logbooks"

        Logs id ->
            "/logbooks/" ++ String.fromInt id

        EditorNew id ->
            "/logbooks/" ++ String.fromInt id ++ "/new"

        EditorEdit id clientId ->
            "/logbooks/" ++ String.fromInt id ++ "/edit/" ++ clientId
