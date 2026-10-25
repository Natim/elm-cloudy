module Session exposing (Session, decoder, encode, toClient)

import Cloudy.Client as Client exposing (Client)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Session =
    { baseUrl : String
    , accessToken : String
    , userId : Int
    , email : String
    }


decoder : Decoder Session
decoder =
    Decode.map4 Session
        (Decode.field "baseUrl" Decode.string)
        (Decode.field "accessToken" Decode.string)
        (Decode.field "userId" Decode.int)
        (Decode.oneOf
            [ Decode.field "email" Decode.string
            , Decode.succeed ""
            ]
        )


encode : Session -> Value
encode session =
    Encode.object
        [ ( "baseUrl", Encode.string session.baseUrl )
        , ( "accessToken", Encode.string session.accessToken )
        , ( "userId", Encode.int session.userId )
        , ( "email", Encode.string session.email )
        ]


toClient : Session -> Client
toClient session =
    Client.withBearer session.accessToken Client.default
        |> (\client -> { client | baseUrl = session.baseUrl })
