module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Loading
import Page
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url
import Url.Builder exposing (string, int, crossOrigin)
import User exposing (User)
import OtpKey exposing (OtpKey)
import Account exposing (Account)
import Viewer

-- MODEL

type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , user : Status User
    , otpKey : Status OtpKey
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
      , otpKey = Loading
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , fetchUser session
            |> Task.attempt CompletedUserLoad
        , fetchOtpKey session
            |> Task.attempt CompletedOtpKeyLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Bouncr"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ] <|
                      case model.user of
                        Loading -> []
                        Loaded user ->
                            [ div [ class "col-md-4" ]
                                  [ img [ src "http://i.pravatar.cc/200" ] []
                                  , viewUserMenu
                                  , viewAdminMenu user.permissions
                                  ]
                            , div [ class "col-md-8" ]
                                  [ viewProfiles user
                                  , div [] <|
                                      case model.otpKey of
                                        Loading -> []
                                        Loaded otpKey -> [ viewOtpKey user otpKey ]
                                        Failed -> [ Loading.error "otpKey"]
                                  ]
                            ]

                        Failed ->
                            [ Loading.error "user" ]
                ]
            ]
    }

viewOtpKey : User -> OtpKey -> Html Msg
viewOtpKey user otpKey =
    case otpKey.key of
        Just key -> viewOtpQrCode user key
        Nothing -> Html.form [ onSubmit SubmittedGenerateOtpKey ]
                   [ button [ class "btn btn-lg btn-primary pull-xs-right"]
                         [ text "Generate OTP key" ]]

viewOtpQrCode : User -> String -> Html Msg
viewOtpQrCode user key =
    let
        url = crossOrigin "otpauth://totp"
              [ Account.toString user.account ]
              [ string "secret" key
              , string "issuer" "bouncr"
              , int    "digits" 6
              ]

        srcUrl = crossOrigin "https://chart.googleapis.com"
              [ "chart" ]
              [ string "cht" "qr"
              , string "chs" "180x180"
              , string "chld" "L|0"
              , string "chl" url
              ]

    in
        img [ src srcUrl ] []

viewProfiles : User -> Html Msg
viewProfiles user =
    div [ class "tab-pane" ]
        [ div [ class "row" ]
              [ div [ class "col-md-6" ]
                    [ label [ for "account" ] [ text "Account" ] ]
              , div [ class "col-md-6" ]
                    [ text (Account.toString user.account) ]
              ]
        , div [ class "row" ]
              [ div [ class "col-md-6" ]
                    [ label [ for "name" ] [ text "Name" ] ]
              , div [ class "col-md-6" ]
                    [ text user.name ]
              ]
        , div [ class "row" ]
              [ div [ class "col-md-6" ]
                    [ label [ for "email" ] [ text "Email" ] ]
              , div [ class "col-md-6" ]
                    [ text user.email ]
              ]
        ]

viewUserMenu : Html Msg
viewUserMenu =
    div [ class "profile-work" ] <|
        List.concat
            [ [ a [ Route.href Route.ChangePassword ]
                [ text "Change password" ] ]
            ]
viewAdminMenu : List String -> Html Msg
viewAdminMenu permissions =
    div [ class "profile-work" ] <|
        List.concat
            [ [ p [ ] [ text "Admin Menu" ] ]
            , viewUserAdmin permissions
            , viewGroupAdmin permissions
            , viewApplicationAdmin permissions
            , viewRoleAdmin permissions
            , viewPermissionAdmin permissions
            , viewAudit permissions
            ]

viewUserAdmin : List String -> List (Html Msg)
viewUserAdmin permissions =
    if List.any (\a -> a == "any_user:read" || a == "user:read")  permissions then
        [ a [ Route.href Route.UserAdmin ] [ text "User"]
        , br [] []
        ]
    else
        []

viewGroupAdmin : List String -> List (Html Msg)
viewGroupAdmin permissions =
    if List.any (\a -> a == "any_group:read" || a == "group:read")  permissions then
        [ a [ Route.href Route.GroupAdmin ] [ text "Group"]
        , br [] []
        ]
    else
        []

viewApplicationAdmin : List String -> List (Html Msg)
viewApplicationAdmin permissions =
    if List.any (\a -> a == "any_application:read" || a == "application:read")  permissions then
        [ a [ Route.href Route.ApplicationAdmin ] [ text "Application"]
        , br [] []
        ]

    else
        []

viewRoleAdmin : List String -> List (Html Msg)
viewRoleAdmin permissions =
    if List.any (\a -> a == "any_role:read" || a == "role:read")  permissions then
        [ a [ Route.href Route.RoleAdmin ] [ text "Role"]
        , br [] []
        ]

    else
        []

viewPermissionAdmin : List String -> List (Html Msg)
viewPermissionAdmin permissions =
    if List.any (\a -> a == "any_permission:read" || a == "permission:read")  permissions then
        [ a [ Route.href Route.PermissionAdmin ] [ text "Permission"]
        , br [] []
        ]
    else
        []

viewAudit : List String -> List (Html Msg)
viewAudit permissions =
    if List.any (\a -> a == "any_user:read") permissions then
        [ a [ Route.href Route.Audit ] [ text "Audit"]
        , br [] []
        ]
    else
        []

-- HTTP

fetchUser: Session -> Task Http.Error (Http.Metadata, MaybeSuccess User)
fetchUser session =
    let
        maybeCred =
            Session.cred session

        maybeViewer =
            Session.viewer session
    in
        case maybeViewer of
            Just viewer ->
                Http.task
                    { method = "GET"
                    , headers = Api.headers maybeCred
                    , url = Api.url
                            ["user", Account.toString (Viewer.account viewer)]
                            [ string "embed" "(permissions,groups)" ]
                    , body = Http.emptyBody
                    , resolver = Api.jsonResolver User.decoder
                    , timeout = Nothing
                    }
            Nothing ->
                Task.fail (Http.BadStatus 401)

fetchOtpKey: Session -> Task Http.Error (Http.Metadata, MaybeSuccess OtpKey)
fetchOtpKey session =
    let
        maybeCred =
            Session.cred session

        maybeViewer =
            Session.viewer session
    in
        case maybeViewer of
            Just viewer ->
                Http.task
                    { method = "GET"
                    , headers = Api.headers maybeCred
                    , url = Api.url
                            ["otp_key"] [ ]
                    , body = Http.emptyBody
                    , resolver = Api.jsonResolver OtpKey.decoder
                    , timeout = Nothing
                    }
            Nothing ->
                Task.fail (Http.BadStatus 401)

putOtpKey : Session -> Task Http.Error (Http.Metadata, MaybeSuccess OtpKey)
putOtpKey session =
    let
        maybeCred =
            Session.cred session

        maybeViewer =
            Session.viewer session
    in
        case maybeViewer of
            Just viewer ->
                Http.task
                    { method = "PUT"
                    , headers = Api.headers maybeCred
                    , url = Api.url
                            ["otp_key"] [ ]
                    , body = Http.emptyBody
                    , resolver = Api.jsonResolver OtpKey.decoder
                    , timeout = Nothing
                    }
            Nothing ->
                Task.fail (Http.BadStatus 401)

deleteOtpKey : Session -> Task Http.Error (Http.Metadata, MaybeSuccess ())
deleteOtpKey session =
    let
        maybeCred =
            Session.cred session

        maybeViewer =
            Session.viewer session
    in
        case maybeViewer of
            Just viewer ->
                Http.task
                    { method = "DELETE"
                    , headers = Api.headers maybeCred
                    , url = Api.url
                            ["otp_key"] [ ]
                    , body = Http.emptyBody
                    , resolver = Api.jsonResolver (Decode.succeed ())
                    , timeout = Nothing
                    }
            Nothing ->
                Task.fail (Http.BadStatus 401)

-- UPDATE

type Msg
    = GotTimeZone Time.Zone
    | GotSession Session
    | SubmittedGenerateOtpKey
    | CompletedUserLoad (Result Http.Error (Http.Metadata, MaybeSuccess User))
    | CompletedOtpKeyLoad (Result Http.Error (Http.Metadata, MaybeSuccess OtpKey))
    | CompletedOtpKeyPut (Result Http.Error (Http.Metadata, MaybeSuccess OtpKey))
    | CompletedOtpKeyDelete (Result Http.Error ())

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        SubmittedGenerateOtpKey ->
            ( model
            , putOtpKey model.session
            |> Task.attempt CompletedOtpKeyPut)

        CompletedUserLoad (Ok (_, res)) ->
            case res of
                Success user ->
                    ( { model | user = Loaded user }, Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | user = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( model, Cmd.none )

        CompletedUserLoad (Err err) ->
            case err of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | user = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | user = Failed }, Cmd.none )
                _ ->
                    ( { model | user = Failed }, Cmd.none )

        CompletedOtpKeyLoad (Ok (_, res)) ->
            case res of
                Success otpKey ->
                    ( { model | otpKey = Loaded otpKey }, Cmd.none )
                Failure problem ->
                    ( model, Cmd.none )

        CompletedOtpKeyLoad (Err err) ->
            case err of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | user = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | user = Failed }, Cmd.none )
                _ ->
                    ( { model | user = Failed }, Cmd.none )

        CompletedOtpKeyPut (Ok (_, res)) ->
            case res of
                Success otpKey ->
                    ( { model | otpKey = Loaded otpKey }, Cmd.none )
                Failure problem ->
                    ( model, Cmd.none )

        CompletedOtpKeyPut (Err err) ->
            case err of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | otpKey = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | otpKey = Failed }, Cmd.none )
                _ ->
                    ( { model | otpKey = Failed }, Cmd.none )

        CompletedOtpKeyDelete (Ok ()) ->
            let
                nothingKey = OtpKey Nothing
            in
                ( { model | otpKey = Loaded nothingKey }, Cmd.none )

        CompletedOtpKeyDelete (Err err) ->
            case err of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | otpKey = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | otpKey = Failed }, Cmd.none )
                _ ->
                    ( { model | otpKey = Failed }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
