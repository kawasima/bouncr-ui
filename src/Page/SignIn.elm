module Page.SignIn exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)

type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type alias Form =
    { account : String
    , password : String
    }

type Token = Token String

init : Session -> (Model, Cmd msg)
init session =
    ( { session = session
      , problems = []
      , form =
          { account = ""
          , password = ""
          }
      }
    , Cmd.none
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
                      [ a [ href "#TODo" ]
                        [ text "Sign up" ]
                      ]
                  , ul [ class "error-messages" ]
                      (List.map viewProblem model.problems)
                  , viewForm model.form
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

viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
              [ input
                    [ placeholder "Account"
                    , onInput EnteredAccount
                    , value form.account
                    ]
                    []
              ]
        , fieldset [ class "form-group" ]
            [ input
                  [ type_ "password"
                  , placeholder "Password"
                  , onInput EnteredPassword
                  , value form.password
                  ]
                  []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Sign in" ]
        ]

 -- UPDATE

type Msg
    = SubmittedForm
    | EnteredAccount String
    | EnteredPassword String
    | CompletedSignIn (Result Http.Error Token)
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Http.send CompletedSignIn (signIn validForm)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )
        EnteredAccount account ->
            updateForm (\form -> { form | account = account }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedSignIn (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
                ({ model | problems = List.append model.problems serverErrors }
                , Cmd.none
                )
        CompletedSignIn (Ok token) ->
            ( model
            , Viewer.store (Viewer.create model.form.account
                                (case token of (Token t) -> t))
            )
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

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

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Account
    , Password
    ]

validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
        case List.concatMap (validateField trimmedForm) fieldsToValidate of
            [] ->
                Ok trimmedForm

            problems ->
                Err problems

validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
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

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { account = String.trim form.account
        , password = String.trim form.password
        }

-- HTTP

signIn : TrimmedForm -> Http.Request Token
signIn (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "account", Encode.string form.account )
                , ( "password", Encode.string form.password )
                ]
                |> Http.jsonBody
    in
        Api.signIn body tokenDecoder

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
