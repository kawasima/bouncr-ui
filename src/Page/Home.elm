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
import User exposing (User)
import Account exposing (Account)
import Viewer

-- MODEL

type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , user: Status User
    }

type Status a
    = Loading
    | Loaded a
    | Failed

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeZone = Time.utc
      , user = Loading
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , fetchUser session
            |> Task.attempt CompletedUserLoad
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
                , div [ class "row" ] <|
                    case model.user of
                        Loading -> []
                        Loaded user ->
                            [ div [ ]
                              [ text (Account.toString user.account) ]
                            ]
                        Failed ->
                            [ Loading.error "user" ]
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

-- HTTP

fetchUser: Session -> Task Http.Error User
fetchUser session =
    let
        maybeCred =
            Session.cred session

        maybeViewer =
            Session.viewer session

        account =
            case maybeViewer of
                Just viewer -> (Viewer.account viewer)
                Nothing -> (Account.anonymous)

        request =
            User.fetch account maybeCred
    in
        Http.toTask request

-- UPDATE

type Msg
    = GotTimeZone Time.Zone
    | GotSession Session
    | CompletedUserLoad (Result Http.Error User)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedUserLoad (Ok user) ->
            ( { model | user = Loaded user }, Cmd.none )

        CompletedUserLoad (Err err) ->
            ( { model | user = Failed }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
