module Model.Group exposing (Group, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode

-- MODEL

type alias Group =
    { id : Int
    , name : String
    , description : String
    }

-- SERIALIZATION

decoder : Decoder Group
decoder =
    Decode.succeed Group
        |> required "id" (Decode.int)
        |> required "name" Decode.string
        |> required "description" Decode.string
