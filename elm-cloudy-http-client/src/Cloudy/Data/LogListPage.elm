module Cloudy.Data.LogListPage exposing (LogListPage, decoder)

import Cloudy.Data.Log as Log exposing (LogEntry)
import Cloudy.Data.LogbookCategory as LogbookCategory exposing (LogbookCategory)
import Json.Decode as Decode exposing (Decoder)


type alias LogListPage =
    { items : List LogEntry
    , nextPageToken : Maybe String
    }


decoder : LogbookCategory -> Decoder LogListPage
decoder category =
    Decode.map2 LogListPage
        (Decode.field "items" (Decode.list (Log.decoder category)))
        (Decode.maybe (Decode.field "next_page_token" Decode.string))
