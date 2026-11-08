module LogSync exposing (Cursor, encode, isReady, later, watermark)

import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import LocalLog exposing (LocalLog)
import LogHelpers


type alias Cursor =
    { complete : Bool
    , updatedAt : String
    }


isReady : Dict String Cursor -> Dict String (List LocalLog) -> Int -> Bool
isReady cursors logs logbookId =
    case Dict.get (key logbookId) cursors of
        Just cursor ->
            cursor.complete

        Nothing ->
            case Dict.get (key logbookId) logs of
                Just (_ :: _) ->
                    True

                _ ->
                    False


watermark : Dict String Cursor -> Dict String (List LocalLog) -> Int -> Maybe String
watermark cursors logs logbookId =
    let
        stored =
            case Dict.get (key logbookId) cursors of
                Just cursor ->
                    cursor.updatedAt

                Nothing ->
                    later "" (maxUpdated (Dict.get (key logbookId) logs |> Maybe.withDefault []))
    in
    if stored == "" then
        Nothing

    else
        Just stored


later : String -> String -> String
later left right =
    if right > left then
        right

    else
        left


encode : Dict String Cursor -> Value
encode cursors =
    cursors
        |> Dict.toList
        |> List.map (\( name, cursor ) -> ( name, encodeCursor cursor ))
        |> Encode.object


encodeCursor : Cursor -> Value
encodeCursor cursor =
    Encode.object
        [ ( "complete", Encode.bool cursor.complete )
        , ( "updatedAt", Encode.string cursor.updatedAt )
        ]


key : Int -> String
key logbookId =
    String.fromInt logbookId


maxUpdated : List LocalLog -> String
maxUpdated logs =
    List.foldl (\log acc -> later acc (LogHelpers.updatedAt log.entry)) "" logs
