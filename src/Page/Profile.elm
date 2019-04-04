module Page.Profile exposing (Model, Msg, decoder, init, update, viewProfiles)

import Api exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Url exposing (Url)

-- MODEL

type Model
    = Model Internals

type alias Internals =
    { session : Session
    , errors : List String
    , user : User
    , isLoading : Bool
    }

init : Session -> User -> Model
init session user =
    Model
        { session = session
        , errors = []
        , user = user
        , isLoading = False
        }

-- VIEW

viewUser =
    div [ class "" ]
        [ p [] User.account ]
