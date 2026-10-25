module Route exposing (Route(..), fromUrl, logbookId, toPath)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string, top)


type Route
    = Login
    | Dashboard
    | Logbooks
    | Logs Int
    | Aircraft Int
    | AircraftNew Int
    | AircraftEdit Int Int
    | EditorNew Int
    | EditorEdit Int String


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Login (s "login")
        , Parser.map Dashboard (s "dashboard")
        , Parser.map Logbooks (s "logbooks")
        , Parser.map AircraftNew (s "logbooks" </> Parser.int </> s "aircraft" </> s "new")
        , Parser.map AircraftEdit (s "logbooks" </> Parser.int </> s "aircraft" </> Parser.int)
        , Parser.map Aircraft (s "logbooks" </> Parser.int </> s "aircraft")
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


basePath : String
basePath =
    "/elm-cloudy"


normalizePath : String -> String
normalizePath path =
    if String.startsWith (basePath ++ "/") path then
        String.dropLeft (String.length basePath) path

    else if path == basePath || path == basePath ++ "/" then
        "/"

    else
        path


toPath : Route -> String
toPath route =
    basePath
        ++ (case route of
                Login ->
                    "/login"

                Dashboard ->
                    "/dashboard"

                Logbooks ->
                    "/logbooks"

                Logs id ->
                    "/logbooks/" ++ String.fromInt id

                Aircraft id ->
                    "/logbooks/" ++ String.fromInt id ++ "/aircraft"

                AircraftNew id ->
                    "/logbooks/" ++ String.fromInt id ++ "/aircraft/new"

                AircraftEdit id aircraftId ->
                    "/logbooks/" ++ String.fromInt id ++ "/aircraft/" ++ String.fromInt aircraftId

                EditorNew id ->
                    "/logbooks/" ++ String.fromInt id ++ "/new"

                EditorEdit id clientId ->
                    "/logbooks/" ++ String.fromInt id ++ "/edit/" ++ clientId
           )


logbookId : Route -> Maybe Int
logbookId route =
    case route of
        Logs id ->
            Just id

        Aircraft id ->
            Just id

        AircraftNew id ->
            Just id

        AircraftEdit id _ ->
            Just id

        EditorNew id ->
            Just id

        EditorEdit id _ ->
            Just id

        Login ->
            Nothing

        Dashboard ->
            Nothing

        Logbooks ->
            Nothing
