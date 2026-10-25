module Cloudy.Error exposing (Error(..), fromHttp)

import Cloudy.Data.ApiErrorBody as ApiErrorBody exposing (ApiErrorBody)
import Http


type Error
    = Network Http.Error
    | Api Int ApiErrorBody
    | Decode String


fromHttp : Http.Error -> Error
fromHttp err =
    Network err
