module Cloudy.Logs exposing (ListQuery, create, get, list, patch, remove, replace)

import Cloudy.Client exposing (Client)
import Cloudy.Data.Log as Log exposing (LogEntry)
import Cloudy.Data.LogListPage as LogListPage exposing (LogListPage)
import Cloudy.Data.LogbookCategory exposing (LogbookCategory)
import Cloudy.Request as Request exposing (Request(..))
import Json.Encode as Encode


type alias ListQuery =
    { limit : Maybe Int
    , pageToken : Maybe String
    , dateFrom : Maybe String
    , dateTo : Maybe String
    }


list : Client -> LogbookCategory -> Int -> ListQuery -> Request LogListPage
list client category logbookId query =
    let
        params =
            List.filterMap identity
                [ Maybe.map (\v -> ( "limit", String.fromInt v )) query.limit
                , Maybe.map (\v -> ( "page_token", v )) query.pageToken
                , Maybe.map (\v -> ( "date_from", v )) query.dateFrom
                , Maybe.map (\v -> ( "date_to", v )) query.dateTo
                ]
    in
    Request.get client [ "logbooks", String.fromInt logbookId, "logs" ] params (LogListPage.decoder category)


get : Client -> LogbookCategory -> Int -> Int -> Request LogEntry
get client category logbookId logId =
    Request.get client [ "logbooks", String.fromInt logbookId, "logs", String.fromInt logId ] [] (Log.responseDecoder category)


create : Client -> Int -> LogEntry -> Request LogEntry
create client logbookId entry =
    Request.post client [ "logbooks", String.fromInt logbookId, "logs" ] (Log.encodeCreate entry) (Log.responseDecoderFromEntry entry)


replace : Client -> LogbookCategory -> Int -> Int -> LogEntry -> Request LogEntry
replace client category logbookId logId entry =
    Request.put client [ "logbooks", String.fromInt logbookId, "logs", String.fromInt logId ] (Log.encodeReplace entry) (Log.responseDecoder category)


patch : Client -> LogbookCategory -> Int -> Int -> Encode.Value -> Request LogEntry
patch client category logbookId logId body =
    Request.patch client [ "logbooks", String.fromInt logbookId, "logs", String.fromInt logId ] body (Log.responseDecoder category)


remove : Client -> Int -> Int -> Request ()
remove client logbookId logId =
    Request.delete client [ "logbooks", String.fromInt logbookId, "logs", String.fromInt logId ]
