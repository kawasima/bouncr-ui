module Page.EmailVerification exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url.Builder exposing (string, int, crossOrigin)
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Viewer exposing (Viewer)

-- MODEL

type alias Model =
    { session : Session
    , problems : List Problem
    }

type Problem
    = ServerError String

init : String -> Session -> (Model, Cmd Msg)
init code session =
    ( { session = session
      , problems = []
      }
    , Cmd.batch
        [ (verifyEmail code)
        |> Task.attempt CompletedVerification
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Verify your email"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
              [ div [ class "row" ]
                [ div [class "col-md-6 offset-md-3 col-xs-12" ]
                  [ h1 [ class "text-xs-center" ] [ text "Verify Email" ]
                  , ul [ class "error-messages" ]
                      (List.map viewProblem model.problems)
                  ]
                ]
              ]
            ]
    }

viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                ServerError str ->
                    str
    in
        li [] [ text errorMessage ]

 -- UPDATE

type Msg
    = CompletedVerification (Result Http.Error (Http.Metadata, MaybeSuccess ()))
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        CompletedVerification (Err error) ->
            case error of
                _ ->
                    ( { model | problems = [ ServerError "server error"] }, Cmd.none )

        CompletedVerification (Ok (_, _)) ->
            ( model
            , Route.replaceUrl (Session.navKey model.session) Route.SignIn )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- HTTP

verifyEmail : String -> Task Http.Error (Http.Metadata, MaybeSuccess ())
verifyEmail code =
    Http.task
        { method = "DELETE"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = Api.url ["user_profile_verification"] [string "code" code]
        , body = Http.emptyBody
        , resolver = Api.jsonResolver (Decode.succeed ())
        , timeout = Nothing
        }

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
