module Cloudy.Auth exposing (me, token)

import Cloudy.Client exposing (Client)
import Cloudy.Data.Profile as Profile exposing (Profile)
import Cloudy.Data.Token as Token exposing (TokenRequest, TokenResponse)
import Cloudy.Request as Request exposing (Request(..))


token : Client -> TokenRequest -> Request TokenResponse
token client credentials =
    Request.post client [ "auth", "token" ] (Token.encodeRequest credentials) Token.decoder


me : Client -> Request Profile
me client =
    Request.get client [ "me" ] [] Profile.decoder
