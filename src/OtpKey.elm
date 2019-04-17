module OtpKey exposing (OtpKey, decoder)

import Json.Decode as Decode exposing (Decoder, string, map)
import Json.Decode.Pipeline exposing (required, optional)

-- MODEL

type alias OtpKey =
    { key : Maybe String }

-- SERIALIZATION

decoder : Decoder OtpKey
decoder =
    Decode.succeed OtpKey
        |> optional "key" (map Just string) Nothing
