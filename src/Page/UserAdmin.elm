module Page.UserAdmin exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
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
    , users: Status (List User)
    }

type Status a
    = Loading
    | Loaded a
    | Failed

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , users = Loading
      }
    , Cmd.batch
        [ fetchUsers session
            |> Task.attempt CompletedUsersLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "User Administration / Bouncr"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                  [ div [ class "row" ] <|
                        case model.users of
                            Loading -> []
                            Loaded users ->
                                [ table [ class "table" ]
                                      [ thead []
                                            [ tr []
                                                  [ th [] [ text "Account" ]
                                                  , th [] [ text "Name" ]
                                                  , th [] [ text "Email" ]
                                                  ]
                                            ]

                                      , tbody []
                                            (List.map viewUser users)
                                      ]
                                ]
                            Failed ->
                                [ Loading.error "user" ]
                  ]
            ]
    }

viewUser : User -> Html Msg
viewUser user =
    tr []
        [ td [ ]
            [ text (Account.toString user.account) ]
        , td [ ]
            [ text user.name ]
        , td [ ]
            [ text user.email ]
        ]

-- HTTP

fetchUsers: Session -> Task Http.Error (List User)
fetchUsers session =
    let
        maybeCred =
            Session.cred session

        decoder =
            Decode.list User.decoder
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["users"] []
            , body = Http.emptyBody
            , resolver = Api.jsonResolver decoder
            , timeout = Nothing
            }


-- UPDATE

type Msg
    = GotSession Session
    | CompletedUsersLoad (Result Http.Error (List User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedUsersLoad (Ok users) ->
            ( { model | users = Loaded users }, Cmd.none )

        CompletedUsersLoad (Err err) ->
            ( { model | users = Failed }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
