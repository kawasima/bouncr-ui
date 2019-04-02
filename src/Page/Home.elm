module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Loading
import Page
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Account exposing (Account)

-- MODEL

type alias Model =
    { session : Session
    , timeZone : Time.Zone
    }

type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeZone = Time.utc
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Bouncr"
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , div [ class "container page" ]
                [ div [ class "row" ]
                  [ text "bouncr" ]
                ]
            ]
    }

viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
          [ h1 [ class "logo-font" ] [ text "Bouncr" ]
          , p [] [ text "bouncr" ]
          ]
        ]

-- UPDATE

type Msg
    = GotTimeZone Time.Zone
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )
        GotSession session ->
            ( { model | session = session }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
