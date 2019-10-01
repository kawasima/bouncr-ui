module Api.Endpoint exposing (Endpoint, url)

import Http
import Url.Builder exposing (QueryParameter, string)
import Account exposing (Account)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect msg
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , tracker : Maybe String
    , url : Endpoint
    }
    -> Cmd msg

request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , tracker = config.tracker
        , url = unwrap config.url
        }



-- TYPESp


{-| Get a URL to the Conduit API.
This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.
-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> String
url paths queryParams =
    Url.Builder.crossOrigin "http://localhost:3000"
        ("bouncr" :: "api" :: paths)
        queryParams

-- ENDPOINTS


-- user : Account -> Endpoint
-- user acc =
--    url [ "user", Account.toString acc ] [ string "embed" "(permissions,groups)"]
