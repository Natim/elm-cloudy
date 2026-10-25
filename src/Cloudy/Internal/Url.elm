module Cloudy.Internal.Url exposing (build)

import Url.Builder


build : String -> List String -> List ( String, String ) -> String
build baseUrl pathSegments query =
    let
        trimmed =
            if String.endsWith "/" baseUrl then
                String.dropRight 1 baseUrl

            else
                baseUrl
    in
    Url.Builder.crossOrigin trimmed pathSegments (List.map (\( key, value ) -> Url.Builder.string key value) query)
