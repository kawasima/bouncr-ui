module Page.SignInByOidc exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Url.Builder exposing (crossOrigin, string)
import Random.String
import Random.Char
import Random
import Viewer exposing (Viewer)

type alias Model =
    { session : Session
    }

init : String -> String -> Session -> (Model, Cmd Msg)
init account token session =
    ( { session = session }
    , Cmd.batch
        [ Viewer.store (Viewer.create account token)
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Sign Up by Oidc"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
              [ div [ class "row" ]
                [ div  [class "col-md-6 offset-md-3 col-xs-12" ]
                  [ h1 [ class "text-xs-center" ] [ text "Signed up by OpenID Connect" ]
                  , p [ class "text-cs-center" ]
                      [ text "Signed up by OpenID Connect" ]
                  ]
                ]
              ]
            ]
    }


type Msg
    = GotToken Viewer
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        GotToken viewer ->
            ( model
            , Viewer.store  viewer
            )
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
