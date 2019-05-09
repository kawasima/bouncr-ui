module Model.InitialPassword exposing (InitialPassword, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)

-- MODEL

type alias InitialPassword =
    { password : String
    }

-- SERIALIZATION

decoder : Decoder InitialPassword
decoder =
    Decode.succeed InitialPassword
        |> required "password" (Decode.string)
