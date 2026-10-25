module Cloudy.Client exposing (Auth(..), Client, authHeader, default, withBearer)

import Http


type Auth
    = NoAuth
    | Bearer String


type alias Client =
    { baseUrl : String
    , auth : Auth
    }


default : Client
default =
    { baseUrl = "https://www.cloudy.fr/api/v1"
    , auth = NoAuth
    }


withBearer : String -> Client -> Client
withBearer token client =
    { client | auth = Bearer token }


authHeader : Client -> List Http.Header
authHeader client =
    case client.auth of
        NoAuth ->
            []

        Bearer token ->
            [ Http.header "Authorization" ("Bearer " ++ token) ]
