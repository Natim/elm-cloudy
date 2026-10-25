module Cloudy.Auth exposing (token)

import Cloudy.Client exposing (Client)
import Cloudy.Data.Token as Token exposing (TokenRequest, TokenResponse)
import Cloudy.Request as Request exposing (Request(..))


token : Client -> TokenRequest -> Request TokenResponse
token client credentials =
    Request.post client [ "auth", "token" ] (Token.encodeRequest credentials) Token.decoder
