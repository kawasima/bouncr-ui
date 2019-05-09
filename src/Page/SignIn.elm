module Page.SignIn exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Url.Builder exposing (crossOrigin, string)
import Random.String
import Random.Char
import Random
import Viewer exposing (Viewer)

type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    , formState : FormState
    , nonce : String
    , state : String
    }

type FormState
    = AccountPassword
    | Otp

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type alias Form =
    { account : String
    , password : String
    , code : String
    }

type Token = Token String

init : Session -> (Model, Cmd Msg)
init session =
    ( { session = session
      , problems = []
      , form =
          { account = ""
          , password = ""
          , code = ""
          }
      , formState = AccountPassword
      , nonce = ""
      , state = ""
      }
    , Cmd.batch
        [ Random.generate GeneratedNonce (Random.String.string 8 Random.Char.upperCaseLatin)
        , Random.generate GeneratedState (Random.String.string 8 Random.Char.upperCaseLatin)
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "SignIn"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
              [ div [ class "row" ]
                [ div [class "col-md-6 offset-md-3 col-xs-12" ]
                  [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                  , p [ class "text-xs-center" ]
                      [ a [ Route.href Route.SignUp ]
                        [ text "Sign up" ]
                      ]
                  , ul [ class "error-messages" ]
                      (List.map viewProblem model.problems)
                  , case model.formState of
                        AccountPassword -> viewAccountPasswordForm model.form
                        Otp -> viewOtpForm model.form
                  , hr [] []
                  , viewSignInByOIDC model.nonce model.state
                  , hr [] []
                  , p []
                      [ a [ Route.href Route.ResetPasswordChallenge]
                        [ text "Forgott password?"]
                      ]
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
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
        li [] [ text errorMessage ]

viewAccountPasswordForm : Form -> Html Msg
viewAccountPasswordForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
              [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Account"
                    , onInput EnteredAccount
                    , value form.account
                    ]
                    []
              ]
        , fieldset [ class "form-group" ]
            [ input
                  [ class "form-control form-control-lg"
                  , type_ "password"
                  , placeholder "Password"
                  , onInput EnteredPassword
                  , value form.password
                  ]
                  []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Sign in" ]
        ]

viewOtpForm : Form -> Html Msg
viewOtpForm form =
    Html.form [ onSubmit SubmittedOtpForm ]
        [ fieldset [ class "form-group" ]
              [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Code"
                    , onInput EnteredCode
                    , value form.code
                    ]
                    []
              ]
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Sign in" ]
        ]

viewSignInByOIDC : String -> String -> Html msg
viewSignInByOIDC nonce state =
    let
        url = crossOrigin "https://accounts.google.com"
              [ "o", "oauth2", "v2" , "auth"]
              [ string "client_id" "706245317584-n6fhhb1hq6vmctku8riab5sbp1h8skqo.apps.googleusercontent.com"
              , string "response_type" "code"
              , string "scope" "openid email"
              , string "redirect_uri" "http://localhost:3000/bouncr/api/sign_in/oidc/google"
              , string "state" state
              , string "nonce" nonce
              ]
    in
        a [href url] [text "Sign in by Google"]

 -- UPDATE

type Msg
    = SubmittedForm
    | SubmittedOtpForm
    | GeneratedNonce String
    | GeneratedState String
    | EnteredAccount String
    | EnteredCode String
    | EnteredPassword String
    | CompletedSignIn (Result Http.Error (Http.Metadata, MaybeSuccess Token))
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form model.formState of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedSignIn (signIn validForm)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )
        SubmittedOtpForm ->
            case validate model.form model.formState of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedSignIn (signIn validForm)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )
        EnteredAccount account ->
            updateForm (\form -> { form | account = account }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        EnteredCode code ->
            updateForm (\form -> { form | code = code }) model

        CompletedSignIn (Err error) ->
            case error of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | problems = [ServerError "Authentication failed"] }, Cmd.none )
                        _ ->
                            ( { model | problems = [ServerError "Server Error"] }, Cmd.none )
                _ ->
                    ( { model | problems = [ServerError "Server Error"] }, Cmd.none )
        CompletedSignIn (Ok token) ->
            case token of
                (_, Success (Token t)) ->
                    ( model
                    , Viewer.store (Viewer.create model.form.account t)
                    )

                (_, Failure problem) ->
                    case problem.type_ of
                        "/bouncr/problem/ONE_TIME_PASSWORD_IS_NEEDED" ->
                            ( { model | formState = Otp }, Cmd.none )
                        _ ->
                            ( { model | problems = [ServerError "Password mismatch"]}, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        GeneratedNonce nonce ->
            ( { model | nonce = nonce }, Cmd.none)

        GeneratedState state ->
            ( { model | state = state }, Cmd.none)

updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- SERIALIZATION

tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> required "token" Decode.string

-- FORM

type TrimmedForm
    = Trimmed Form

type ValidatedField
    = Account
    | Password
    | Code

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Account
    , Password
    , Code
    ]

validate : Form -> FormState -> Result (List Problem) TrimmedForm
validate form formState =
    let
        trimmedForm =
            trimFields form
    in
        case List.concatMap (validateField trimmedForm formState) fieldsToValidate of
            [] ->
                Ok trimmedForm

            problems ->
                Err problems

validateField : TrimmedForm -> FormState -> ValidatedField -> List Problem
validateField (Trimmed form) formState field =
    List.map(InvalidEntry field) <|
        case field of
            Account ->
                if String.isEmpty form.account then
                    [ "account can't be blank." ]
                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]
                else
                    []

            Code ->
                if formState == Otp && String.isEmpty form.code then
                    [ "code can't be blank." ]
                else
                    []

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { account = String.trim form.account
        , password = String.trim form.password
        , code = String.trim form.code
        }

-- HTTP

signIn : TrimmedForm -> Task Http.Error (Http.Metadata, MaybeSuccess Token)
signIn (Trimmed form) =
    let
        code =
            if String.isEmpty form.code
            then []
            else [ ("one_time_password", Encode.string form.code ) ]
        body =
            Encode.object
                ([( "account", Encode.string form.account )
                 , ( "password", Encode.string form.password )
                 ] ++ code
                ) |> Http.jsonBody
    in
        Http.task
            { method = "POST"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = Api.url ["sign_in"] []
            , body = body
            , resolver = Api.jsonResolver tokenDecoder
            , timeout = Nothing
            }

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
