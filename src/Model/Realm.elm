module Model.Realm exposing (Realm, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

-- MODEL

type alias Realm =
    { id : Int
    , name : String
    , description : String
    , url : String
    }

-- SERIALIZATION

decoder : Decoder Realm
decoder =
    Decode.succeed Realm
        |> required "id" (Decode.int)
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "url" Decode.string
