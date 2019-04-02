module Viewer exposing (Viewer, cred, minPasswordChars, store, account, decoder)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Api exposing (Cred)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Account exposing (Account)



-- TYPES


type Viewer
    = Viewer Cred



-- INFO


cred : Viewer -> Cred
cred (Viewer val) =
    val


account : Viewer -> Account
account (Viewer val) =
    Api.account val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    6



-- SERIALIZATION

decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer

store : Viewer -> Cmd msg
store (Viewer credVal) =
    Api.storeCredWith
        credVal
