module Model.Permission exposing (Permission, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

-- MODEL

type alias Permission =
    { id : Int
    , name : String
    , description : String
    }

-- SERIALIZATION

decoder : Decoder Permission
decoder =
    Decode.succeed Permission
        |> required "id" (Decode.int)
        |> required "name" Decode.string
        |> required "description" Decode.string
