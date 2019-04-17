module Main exposing (main)

import Api exposing (Cred, MaybeSuccess(..))
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Blank as Blank
import Page.NotFound as NotFound
import Page.Home as Home
import Page.SignIn as SignIn
import Page.SignUp as SignUp
import Page.ChangePassword as ChangePassword
import Page.UserAdmin as UserAdmin
import Page.PermissionAdmin as PermissionAdmin
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url exposing (Url)
import Account exposing (Account)
import Viewer exposing (Viewer)

type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | SignIn SignIn.Model
    | SignUp SignUp.Model
    | ChangePassword ChangePassword.Model
    | UserAdmin UserAdmin.Model
    | PermissionAdmin PermissionAdmin.Model

-- MODEL

init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg)
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))

-- VIEW

view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (Session.viewer (toSession model)) page config
            in
                { title = title
                , body = List.map (Html.map toMsg) body
                }
    in
        case model of
            Redirect _ ->
                viewPage Page.Other (\_ -> Ignored) Blank.view

            NotFound _ ->
                viewPage Page.Other (\_ -> Ignored) NotFound.view

            Home home ->
                viewPage Page.Home GotHomeMsg (Home.view home)

            SignIn signIn ->
                viewPage Page.Other GotSignInMsg (SignIn.view signIn)

            SignUp signUp ->
                viewPage Page.Other GotSignUpMsg (SignUp.view signUp)

            ChangePassword changePassword ->
                viewPage Page.Other GotChangePasswordMsg (ChangePassword.view changePassword)

            UserAdmin userAdmin ->
                viewPage Page.Other GotUserAdminMsg (UserAdmin.view userAdmin)

            PermissionAdmin permissionAdmin ->
                viewPage Page.Other GotPermissionAdminMsg (PermissionAdmin.view permissionAdmin)

-- HTTP

signOut : Session -> Task Http.Error (Http.Metadata, MaybeSuccess ())
signOut session =
    let
        maybeCred = Session.cred session

        token = case maybeCred of
                    Just t -> t
                    Nothing -> Api.credFromToken("dummy") -- FIXME
    in
    Http.task
        { method = "DELETE"
        , headers = Api.headers maybeCred
        , url = Api.url ["session", Api.token(token) ] []
        , body = Http.emptyBody
        , resolver = Api.jsonResolver (Decode.succeed ())
        , timeout = Nothing
        }

-- UPDATE

type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSignInMsg SignIn.Msg
    | GotSignUpMsg SignUp.Msg
    | GotChangePasswordMsg ChangePassword.Msg
    | GotUserAdminMsg UserAdmin.Msg
    | GotPermissionAdminMsg PermissionAdmin.Msg
    | GotSession Session
    | SignedOut (Result Http.Error (Http.Metadata, MaybeSuccess ()))

toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        SignIn signIn ->
            SignIn.toSession signIn

        SignUp signUp ->
            SignUp.toSession signUp

        ChangePassword changePassword ->
            ChangePassword.toSession changePassword

        UserAdmin userAdmin ->
            UserAdmin.toSession userAdmin

        PermissionAdmin permissionAdmin ->
            PermissionAdmin.toSession permissionAdmin

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
        case maybeRoute of
            Nothing ->
                ( NotFound session, Cmd.none )

            Just Route.Root ->
                ( model, Route.replaceUrl (Session.navKey session) Route.Home)

            Just Route.SignOut ->
                ( model, Task.attempt SignedOut (signOut session) )

            Just Route.Home ->
                Home.init session
                    |> updateWith Home GotHomeMsg model

            Just Route.SignIn ->
                SignIn.init session
                    |> updateWith SignIn GotSignInMsg model

            Just Route.SignUp ->
                SignUp.init session
                    |> updateWith SignUp GotSignUpMsg model

            Just Route.ChangePassword ->
                ChangePassword.init session
                    |> updateWith ChangePassword GotChangePasswordMsg model

            Just Route.UserAdmin ->
                UserAdmin.init session
                    |> updateWith UserAdmin GotUserAdminMsg model

            Just Route.PermissionAdmin ->
                PermissionAdmin.init session
                    |> updateWith PermissionAdmin GotPermissionAdminMsg model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        (Ignored, _ ) ->
            ( model, Cmd.none )
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )
                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )
                Browser.External href ->
                    ( model
                    , Nav.load href
                    )
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotSignInMsg subMsg, SignIn signIn ) ->
            SignIn.update subMsg signIn
                |> updateWith SignIn GotSignInMsg model

        ( GotSignUpMsg subMsg, SignUp signUp ) ->
            SignUp.update subMsg signUp
                |> updateWith SignUp GotSignUpMsg model

        ( GotChangePasswordMsg subMsg, ChangePassword changePassword ) ->
            ChangePassword.update subMsg changePassword
                |> updateWith ChangePassword GotChangePasswordMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotUserAdminMsg subMsg, UserAdmin userAdmin ) ->
            UserAdmin.update subMsg userAdmin
                |> updateWith UserAdmin GotUserAdminMsg model

        ( GotPermissionAdminMsg subMsg, PermissionAdmin permissionAdmin ) ->
            PermissionAdmin.update subMsg permissionAdmin
                |> updateWith PermissionAdmin GotPermissionAdminMsg model

        ( GotSession session, Redirect _ ) ->
            (Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )
        ( SignedOut _, _) ->
            ( model, Cmd.batch
                  [ Api.signOut
                  , Route.replaceUrl (Session.navKey (toSession model)) Route.Home ] )
        ( _, _ ) ->
            ( model, Cmd.none )

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        SignIn signIn ->
            Sub.map GotSignInMsg (SignIn.subscriptions signIn)

        SignUp signUp ->
            Sub.map GotSignUpMsg (SignUp.subscriptions signUp)

        ChangePassword changePassword ->
            Sub.map GotChangePasswordMsg (ChangePassword.subscriptions changePassword)

        UserAdmin userAdmin ->
            Sub.map GotUserAdminMsg (UserAdmin.subscriptions userAdmin)

        PermissionAdmin permissionAdmin ->
            Sub.map GotPermissionAdminMsg (PermissionAdmin.subscriptions permissionAdmin)

-- MAIN

main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
