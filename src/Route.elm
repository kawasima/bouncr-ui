module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query
import Account exposing (Account)


-- ROUTING


type Route
    = Home
    | Root
    | SignIn
    | SignOut
    | SignUp
    | ChangePassword
    | ResetPasswordChallenge
    | ResetPassword (Maybe String)
    | UserAdmin
    | GroupAdmin
    | ApplicationAdmin
    | RoleAdmin
    | PermissionAdmin
    | Audit

parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map SignIn (s "sign_in")
        , Parser.map SignOut (s "sign_out")
        , Parser.map SignUp (s "sign_up")
        , Parser.map ChangePassword (s "change_password")
        , Parser.map ResetPasswordChallenge (s "reset_password_challenge")
        , Parser.map ResetPassword (s "reset_password" <?> Query.string "code")
        , Parser.map UserAdmin (s "user_admin")
        , Parser.map GroupAdmin (s "group_admin")
        , Parser.map ApplicationAdmin (s "application_admin")
        , Parser.map RoleAdmin (s "role_admin")
        , Parser.map PermissionAdmin (s "permission_admin")
        , Parser.map Audit (s "audit")
        ]

-- PUBLIC HELPERS

href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)

fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url
        | path = Maybe.withDefault "" url.fragment
        , query = url.query
        , fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL

routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                SignIn ->
                    [ "sign_in" ]

                SignOut ->
                    [ "sign_out" ]

                SignUp ->
                    [ "sign_up" ]

                ChangePassword ->
                    [ "change_password" ]

                ResetPasswordChallenge ->
                    [ "reset_password_challenge" ]

                ResetPassword (Just code) ->
                    [ "reset_password", code ]

                ResetPassword Nothing ->
                    [ "reset_password" ]

                UserAdmin ->
                    [ "user_admin" ]

                GroupAdmin ->
                    [ "group_admin" ]

                ApplicationAdmin ->
                    [ "application_admin" ]

                RoleAdmin ->
                    [ "role_admin" ]

                PermissionAdmin ->
                    [ "permission_admin" ]

                Audit ->
                    [ "audit" ]

    in
    "#/" ++ String.join "/" pieces
