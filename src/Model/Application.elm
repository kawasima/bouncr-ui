module Model.Application exposing (Application, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Model.Realm as Realm exposing (Realm)

-- MODEL

type alias Application =
    { id : Int
    , name : String
    , description : String
    , pass_to : String
    , virtual_path : String
    , top_page : String
    , realms : List Realm
    }

-- SERIALIZATION

decoder : Decoder Application
decoder =
    Decode.succeed Application
        |> required "id" (Decode.int)
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "pass_to" Decode.string
        |> required "virtual_path" Decode.string
        |> required "top_page" Decode.string
        |> required "realms" (Decode.list Realm.decoder)
