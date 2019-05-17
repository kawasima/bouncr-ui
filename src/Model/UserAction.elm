module Model.UserAction exposing (UserAction, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Time
import Iso8601

-- MODEL

type alias UserAction =
    { id : Int
    , actionType : String
    , actor : String
    , actorIp : String
    , options : Maybe String
    , createdAt : Time.Posix
    }

-- SERIALIZATION

decoder : Decoder UserAction
decoder =
    Decode.succeed UserAction
        |> required "id" (Decode.int)
        |> required "actionType" Decode.string
        |> required "actor" Decode.string
        |> required "actor_ip" Decode.string
        |> optional "options" (Decode.map Just Decode.string) Nothing
        |> required "created_at" Iso8601.decoder
