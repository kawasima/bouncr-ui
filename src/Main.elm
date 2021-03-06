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
import Page.SignInByOidc as SignInByOidc
import Page.SignUp as SignUp
import Page.ChangePassword as ChangePassword
import Page.ChangeProfile as ChangeProfile
import Page.ResetPasswordChallenge as ResetPasswordChallenge
import Page.ResetPassword as ResetPassword
import Page.UserAdmin as UserAdmin
import Page.GroupAdmin as GroupAdmin
import Page.ApplicationAdmin as ApplicationAdmin
import Page.RoleAdmin as RoleAdmin
import Page.PermissionAdmin as PermissionAdmin
import Page.Audit as Audit
import Page.EmailVerification as EmailVerification
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
    | SignInByOidc SignInByOidc.Model
    | SignUp SignUp.Model
    | ChangePassword ChangePassword.Model
    | ChangeProfile ChangeProfile.Model
    | ResetPasswordChallenge ResetPasswordChallenge.Model
    | ResetPassword ResetPassword.Model
    | UserAdmin UserAdmin.Model
    | GroupAdmin GroupAdmin.Model
    | ApplicationAdmin ApplicationAdmin.Model
    | RoleAdmin RoleAdmin.Model
    | PermissionAdmin PermissionAdmin.Model
    | Audit Audit.Model
    | EmailVerification EmailVerification.Model

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

            SignInByOidc signInByOidc ->
                viewPage Page.Other GotSignInByOidcMsg (SignInByOidc.view signInByOidc)

            SignUp signUp ->
                viewPage Page.Other GotSignUpMsg (SignUp.view signUp)

            ChangePassword changePassword ->
                viewPage Page.Other GotChangePasswordMsg (ChangePassword.view changePassword)

            ChangeProfile changeProfile ->
                viewPage Page.Other GotChangeProfileMsg (ChangeProfile.view changeProfile)

            ResetPasswordChallenge resetPasswordChallenge ->
                viewPage Page.Other GotResetPasswordChallengeMsg (ResetPasswordChallenge.view resetPasswordChallenge)

            ResetPassword resetPassword ->
                viewPage Page.Other GotResetPasswordMsg (ResetPassword.view resetPassword)

            UserAdmin userAdmin ->
                viewPage Page.Other GotUserAdminMsg (UserAdmin.view userAdmin)

            GroupAdmin groupAdmin ->
                viewPage Page.Other GotGroupAdminMsg (GroupAdmin.view groupAdmin)

            ApplicationAdmin applicationAdmin ->
                viewPage Page.Other GotApplicationAdminMsg (ApplicationAdmin.view applicationAdmin)

            RoleAdmin roleAdmin ->
                viewPage Page.Other GotRoleAdminMsg (RoleAdmin.view roleAdmin)

            PermissionAdmin permissionAdmin ->
                viewPage Page.Other GotPermissionAdminMsg (PermissionAdmin.view permissionAdmin)

            Audit audit ->
                viewPage Page.Other GotAuditMsg (Audit.view audit)

            EmailVerification emailVerification ->
                viewPage Page.Other GotEmailVerificationMsg (EmailVerification.view emailVerification)

-- HTTP

signOut : Session -> Task Http.Error (Http.Metadata, MaybeSuccess ())
signOut session =
    let
        maybeCred = Session.cred session

        token = case maybeCred of
                    Just t -> t
                    Nothing -> Api.credFromToken("dummy") -- FIXME
    in
    Http.riskyTask
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
    | GotSignInByOidcMsg SignInByOidc.Msg
    | GotSignUpMsg SignUp.Msg
    | GotChangePasswordMsg ChangePassword.Msg
    | GotChangeProfileMsg ChangeProfile.Msg
    | GotResetPasswordChallengeMsg ResetPasswordChallenge.Msg
    | GotResetPasswordMsg ResetPassword.Msg
    | GotUserAdminMsg UserAdmin.Msg
    | GotGroupAdminMsg GroupAdmin.Msg
    | GotApplicationAdminMsg ApplicationAdmin.Msg
    | GotRoleAdminMsg RoleAdmin.Msg
    | GotPermissionAdminMsg PermissionAdmin.Msg
    | GotAuditMsg Audit.Msg
    | GotEmailVerificationMsg EmailVerification.Msg
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

        SignInByOidc signInByOidc ->
            SignInByOidc.toSession signInByOidc

        SignUp signUp ->
            SignUp.toSession signUp

        ChangePassword changePassword ->
            ChangePassword.toSession changePassword

        ChangeProfile changeProfile ->
            ChangeProfile.toSession changeProfile

        ResetPasswordChallenge resetPasswordChallenge ->
            ResetPasswordChallenge.toSession resetPasswordChallenge

        ResetPassword resetPassword ->
            ResetPassword.toSession resetPassword

        UserAdmin userAdmin ->
            UserAdmin.toSession userAdmin

        GroupAdmin groupAdmin ->
            GroupAdmin.toSession groupAdmin

        ApplicationAdmin applicationAdmin ->
            ApplicationAdmin.toSession applicationAdmin

        RoleAdmin roleAdmin ->
            RoleAdmin.toSession roleAdmin

        PermissionAdmin permissionAdmin ->
            PermissionAdmin.toSession permissionAdmin

        Audit audit ->
            Audit.toSession audit

        EmailVerification emailVerification ->
            EmailVerification.toSession emailVerification

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

            Just (Route.SignInByOidc maybeAccount maybeToken) ->
                let
                    account = case maybeAccount of
                                  Just acc -> acc
                                  Nothing  -> ""
                    token = case maybeToken of
                                Just t -> t
                                Nothing  -> ""
                in
                    SignInByOidc.init account token session
                        |> updateWith SignInByOidc GotSignInByOidcMsg model

            Just (Route.SignUp maybeCode) ->
                (SignUp.init
                     (case maybeCode of
                          Just code -> code
                          Nothing -> "")
                     session )
                    |> updateWith SignUp GotSignUpMsg model

            Just Route.ChangePassword ->
                ChangePassword.init session
                    |> updateWith ChangePassword GotChangePasswordMsg model

            Just Route.ChangeProfile ->
                ChangeProfile.init session
                    |> updateWith ChangeProfile GotChangeProfileMsg model

            Just Route.ResetPasswordChallenge ->
                ResetPasswordChallenge.init session
                    |> updateWith ResetPasswordChallenge GotResetPasswordChallengeMsg model

            Just (Route.ResetPassword maybeCode) ->
                (ResetPassword.init
                     (case maybeCode of
                         Just code -> code
                         Nothing -> "")
                     session )
                    |> updateWith ResetPassword GotResetPasswordMsg model

            Just Route.UserAdmin ->
                UserAdmin.init session
                    |> updateWith UserAdmin GotUserAdminMsg model

            Just Route.GroupAdmin ->
                GroupAdmin.init session
                    |> updateWith GroupAdmin GotGroupAdminMsg model

            Just Route.ApplicationAdmin ->
                ApplicationAdmin.init session
                    |> updateWith ApplicationAdmin GotApplicationAdminMsg model

            Just Route.RoleAdmin ->
                RoleAdmin.init session
                    |> updateWith RoleAdmin GotRoleAdminMsg model

            Just Route.PermissionAdmin ->
                PermissionAdmin.init session
                    |> updateWith PermissionAdmin GotPermissionAdminMsg model

            Just Route.Audit ->
                Audit.init session
                    |> updateWith Audit GotAuditMsg model

            Just (Route.EmailVerification maybeCode) ->
                (EmailVerification.init
                     (case maybeCode of
                         Just code -> code
                         Nothing -> "")
                     session )
                    |> updateWith EmailVerification GotEmailVerificationMsg model

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

        ( GotSignInByOidcMsg subMsg, SignInByOidc signInByOidc ) ->
            SignInByOidc.update subMsg signInByOidc
                |> updateWith SignInByOidc GotSignInByOidcMsg model

        ( GotSignUpMsg subMsg, SignUp signUp ) ->
            SignUp.update subMsg signUp
                |> updateWith SignUp GotSignUpMsg model

        ( GotChangePasswordMsg subMsg, ChangePassword changePassword ) ->
            ChangePassword.update subMsg changePassword
                |> updateWith ChangePassword GotChangePasswordMsg model

        ( GotChangeProfileMsg subMsg, ChangeProfile changeProfile ) ->
            ChangeProfile.update subMsg changeProfile
                |> updateWith ChangeProfile GotChangeProfileMsg model

        ( GotResetPasswordChallengeMsg subMsg, ResetPasswordChallenge resetPasswordChallenge ) ->
            ResetPasswordChallenge.update subMsg resetPasswordChallenge
                |> updateWith ResetPasswordChallenge GotResetPasswordChallengeMsg model

        ( GotResetPasswordMsg subMsg, ResetPassword resetPassword ) ->
            ResetPassword.update subMsg resetPassword
                |> updateWith ResetPassword GotResetPasswordMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotUserAdminMsg subMsg, UserAdmin userAdmin ) ->
            UserAdmin.update subMsg userAdmin
                |> updateWith UserAdmin GotUserAdminMsg model

        ( GotGroupAdminMsg subMsg, GroupAdmin groupAdmin ) ->
            GroupAdmin.update subMsg groupAdmin
                |> updateWith GroupAdmin GotGroupAdminMsg model

        ( GotApplicationAdminMsg subMsg, ApplicationAdmin applicationAdmin ) ->
            ApplicationAdmin.update subMsg applicationAdmin
                |> updateWith ApplicationAdmin GotApplicationAdminMsg model

        ( GotRoleAdminMsg subMsg, RoleAdmin roleAdmin ) ->
            RoleAdmin.update subMsg roleAdmin
                |> updateWith RoleAdmin GotRoleAdminMsg model

        ( GotPermissionAdminMsg subMsg, PermissionAdmin permissionAdmin ) ->
            PermissionAdmin.update subMsg permissionAdmin
                |> updateWith PermissionAdmin GotPermissionAdminMsg model

        ( GotAuditMsg subMsg, Audit audit ) ->
            Audit.update subMsg audit
                |> updateWith Audit GotAuditMsg model

        ( GotEmailVerificationMsg subMsg, EmailVerification emailVerification ) ->
            EmailVerification.update subMsg emailVerification
                |> updateWith EmailVerification GotEmailVerificationMsg model

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

        SignInByOidc signInByOidc ->
            Sub.map GotSignInByOidcMsg (SignInByOidc.subscriptions signInByOidc)

        SignUp signUp ->
            Sub.map GotSignUpMsg (SignUp.subscriptions signUp)

        ChangePassword changePassword ->
            Sub.map GotChangePasswordMsg (ChangePassword.subscriptions changePassword)

        ChangeProfile changeProfile ->
            Sub.map GotChangeProfileMsg (ChangeProfile.subscriptions changeProfile)

        ResetPasswordChallenge resetPasswordChallenge ->
            Sub.map GotResetPasswordChallengeMsg (ResetPasswordChallenge.subscriptions resetPasswordChallenge)

        ResetPassword resetPassword ->
            Sub.map GotResetPasswordMsg (ResetPassword.subscriptions resetPassword)

        UserAdmin userAdmin ->
            Sub.map GotUserAdminMsg (UserAdmin.subscriptions userAdmin)

        GroupAdmin groupAdmin ->
            Sub.map GotGroupAdminMsg (GroupAdmin.subscriptions groupAdmin)

        ApplicationAdmin applicationAdmin ->
            Sub.map GotApplicationAdminMsg (ApplicationAdmin.subscriptions applicationAdmin)

        RoleAdmin roleAdmin ->
            Sub.map GotRoleAdminMsg (RoleAdmin.subscriptions roleAdmin)

        PermissionAdmin permissionAdmin ->
            Sub.map GotPermissionAdminMsg (PermissionAdmin.subscriptions permissionAdmin)

        Audit audit ->
            Sub.map GotAuditMsg (Audit.subscriptions audit)

        EmailVerification emailVerification ->
            Sub.map GotEmailVerificationMsg (EmailVerification.subscriptions emailVerification)

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
