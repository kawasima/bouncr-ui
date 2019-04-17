module Model.Role exposing (Role, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

-- MODEL

type alias Role =
    { id : Int
    , name : String
    , description : String
    }

-- SERIALIZATION

decoder : Decoder Role
decoder =
    Decode.succeed Role
        |> required "id" (Decode.int)
        |> required "name" Decode.string
        |> required "description" Decode.string
