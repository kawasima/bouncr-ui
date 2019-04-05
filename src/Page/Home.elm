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
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder exposing (string)
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
                              [ text (Account.toString user.account)
                              , div [] (List.map viewPermission user.permissions)
                              , div [] <|
                                  List.concat
                                      [ viewUserAdmin user.permissions
                                      , viewGroupAdmin user.permissions
                                      , viewApplicationAdmin user.permissions
                                      , viewRoleAdmin user.permissions
                                      , viewPermissionAdmin user.permissions
                                      ]


                              ]

                            ]
                        Failed ->
                            [ Loading.error "user" ]
                ]
            ]
    }

viewPermission : String -> Html Msg
viewPermission permission =
    text permission

viewUserAdmin : List String -> List (Html Msg)
viewUserAdmin permissions =
    if List.any (\a -> a == "any_user:read" || a == "user:read")  permissions then
        [li []
             [a [ Route.href Route.UserAdmin ] [ text "User"]]]
    else
        []

viewGroupAdmin : List String -> List (Html Msg)
viewGroupAdmin permissions =
    if List.any (\a -> a == "any_group:read" || a == "group:read")  permissions then
        [li []
             [a [] [ text "Group"]]]
    else
        []

viewApplicationAdmin : List String -> List (Html Msg)
viewApplicationAdmin permissions =
    if List.any (\a -> a == "any_application:read" || a == "application:read")  permissions then
        [li []
             [a [] [ text "Application"]]]
    else
        []

viewRoleAdmin : List String -> List (Html Msg)
viewRoleAdmin permissions =
    if List.any (\a -> a == "any_role:read" || a == "role:read")  permissions then
        [li []
             [a [] [ text "Role"]]]
    else
        []

viewPermissionAdmin : List String -> List (Html Msg)
viewPermissionAdmin permissions =
    if List.any (\a -> a == "any_permission:read" || a == "permission:read")  permissions then
        [li []
             [a [] [ text "Permission"]]]
    else
        []

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
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["user", Account.toString account] [ string "embed" "(permissions,groups)" ]
            , body = Http.emptyBody
            , resolver = Api.jsonResolver User.decoder
            , timeout = Nothing
            }

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
            case err of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | user = Failed }, Cmd.none )
                        _ ->
                            ( { model | user = Failed }, Cmd.none )
                _ ->
                    ( { model | user = Failed }, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
