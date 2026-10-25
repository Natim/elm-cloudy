module Cloudy.Data.AttachedFile exposing (AttachedFile, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias AttachedFile =
    { id : Int
    , url : String
    }


decoder : Decoder AttachedFile
decoder =
    Decode.map2 AttachedFile
        (Decode.field "id" Decode.int)
        (Decode.field "url" Decode.string)


encoder : AttachedFile -> Value
encoder file =
    Encode.object
        [ ( "id", Encode.int file.id )
        , ( "url", Encode.string file.url )
        ]
