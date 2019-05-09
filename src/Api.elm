port module Api exposing (Cred, MaybeSuccess(..), url, headers, token, credFromToken, addServerError, application, signOut, storeCredWith, viewerChanges, jsonResolver)

import Url.Builder exposing (QueryParameter, string)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string, map)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url exposing (Url)
import Account exposing (Account)

-- URL

url : List String -> List QueryParameter -> String
url paths queryParams =
    Url.Builder.crossOrigin "http://localhost:3000"
        ("bouncr" :: "api" :: paths)
        queryParams

-- Problem

type alias Violation =
    { field : String
    , message : String
    }

type alias Problem =
    { type_ : String
    , status : Int
    , violations : Maybe (List Violation)
    }
-- CRED


{-| The authentication credentials for the Viewer (that is, the currently logged-in user.)
This includes:
  - The cred's Username
  - The cred's authentication token
By design, there is no way to access the token directly as a String.
It can be encoded for persistence, and it can be added to a header
to a HttpBuilder for a request, but that's it.
This token should never be rendered to the end user, and with this API, it
can't be!
-}
type Cred
    = Cred String

token : Cred -> String
token (Cred t) =
    t

credFromToken : String -> Cred
credFromToken t =
    (Cred t)

credHeader : Cred -> Http.Header
credHeader (Cred str) =
    Http.header "authorization" ("Bearer " ++ str)

headers : Maybe Cred -> List Http.Header
headers maybeCred =
    List.concat
        [
         [ Http.header "accept" "application/json"
         ]
        , case maybeCred of
              Just cred -> [ credHeader cred ]
              Nothing -> []
        ]

{-| It's important that this is never exposed!
We epxose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.
-}
credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "token" Decode.string


-- PERSISTENCE

decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (decoderFromCred decoder) str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Cred -> Account -> Cmd msg
storeCredWith (Cred t) acc =
    let
        json =
            Encode.object
                [ ( "account", Account.encode acc )
                , ( "token", Encode.string t )
                ]
    in
    storeCache (Just json)


signOut : Cmd msg
signOut =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg


-- APPLICATION


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg

application viewerDecoder config =
    let
        init flags u navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer u navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    decoderFromCred viewerDecoder



-- HTTP

decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder

violationDecoder : Decoder Violation
violationDecoder =
    Decode.succeed Violation
        |> required "field" Decode.string
        |> required "message" Decode.string

problemDecoder : Decoder Problem
problemDecoder =
    Decode.succeed Problem
        |> required "type" Decode.string
        |> required "status" Decode.int
        |> optional "violations" (map Just (Decode.list violationDecoder)) Nothing

type MaybeSuccess a
    = Success a
    | Failure Problem

jsonResolver : Decoder a -> Http.Resolver Http.Error (Http.Metadata, MaybeSuccess a)
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ u ->
                    Err (Http.BadUrl u)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString problemDecoder body of
                        Ok value ->
                            Ok (metadata, Failure value)

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))

                Http.GoodStatus_ metadata body_ ->
                    let
                        body = if String.isEmpty body_ then
                                   "{}"
                               else
                                   body_
                    in
                        case Decode.decodeString decoder body of
                            Ok value ->
                                Ok (metadata, Success value)

                            Err err ->
                                Err (Http.BadBody (Decode.errorToString err))

-- ERRORS

addServerError : List String -> List String
addServerError list =
    "Server error" :: list


{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS


cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"
