module Account exposing (Account, decoder, encode, toHtml, toString, urlParser, anonymous, create)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser

-- TYPES

type Account
    = Account String

-- CREATE

create : String -> Account
create s =
    (Account s)

anonymous : Account
anonymous =
    (Account "")

decoder : Decoder Account
decoder =
    Decode.map Account Decode.string

-- TRANSFORM

encode : Account -> Value
encode (Account account) =
    Encode.string account

toString : Account -> String
toString (Account account) =
    account

urlParser : Url.Parser.Parser (Account -> a) a
urlParser =
    Url.Parser.custom "ACCOUNT" (\str -> Just (Account str))

toHtml : Account -> Html msg
toHtml (Account account) =
    Html.text account
