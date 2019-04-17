module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Account exposing (Account)


-- ROUTING


type Route
    = Home
    | Root
    | SignIn
    | SignOut
    | SignUp
    | ChangePassword
    | UserAdmin
    | PermissionAdmin

parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map SignIn (s "sign_in")
        , Parser.map SignOut (s "sign_out")
        , Parser.map SignUp (s "sign_up")
        , Parser.map ChangePassword (s "change_password")
        , Parser.map UserAdmin (s "user_admin")
        , Parser.map PermissionAdmin (s "permission_admin")
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
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
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

                UserAdmin ->
                    [ "user_admin" ]

                PermissionAdmin ->
                    [ "permission_admin" ]

    in
    "#/" ++ String.join "/" pieces
