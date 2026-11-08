module Cloudy.Data.Profile exposing (Profile, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Profile =
    { id : Int
    , email : String
    , avatarUrl : Maybe String
    }


decoder : Decoder Profile
decoder =
    Decode.map3 Profile
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "avatar_url" (Decode.nullable Decode.string))
