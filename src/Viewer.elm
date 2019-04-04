module Viewer exposing (Viewer, account, cred, minPasswordChars, store, decoder, create)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Api exposing (Cred, credFromToken)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Account exposing (Account)

-- TYPES

type Viewer
    = Viewer Account Cred

create : String -> String -> Viewer
create acc token =
    (Viewer (Account.create acc) (credFromToken token))

-- INFO

cred : Viewer -> Cred
cred (Viewer _ val) =
    val

account : Viewer -> Account
account (Viewer val _) =
    val

{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION

decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "account" Account.decoder)

store : Viewer -> Cmd msg
store (Viewer accountVal credVal) =
    Api.storeCredWith
        credVal
        accountVal
