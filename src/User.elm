module User exposing (User, decoder)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Http
import Session exposing (Session)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

import Account exposing (Account)

-- MODEL

type alias User =
    { id : Int
    , account : Account
    , email : String
    , name : String
    , permissions : List String
    }

-- SERIALIZATION

decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "id" (Decode.int)
        |> required "account" (Account.decoder)
        |> required "email" Decode.string
        |> required "name" Decode.string
        |> optional "permissions" (Decode.list Decode.string) []
