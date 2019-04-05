module Page.SignUp exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)

-- MODEL

type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }

type alias Form =
    { account : String
    , name : String
    , email : String
    , password : String
    }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = []
      , form =
          { account = ""
          , name = ""
          , email = ""
          , password = ""
          }
      }
    , Cmd.none
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Sign In"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
              [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                  [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                  , p [ class "text-cs-center" ]
                      [ a [ Route.href Route.SignIn ]
                        [ text "Hav an account?" ]
                      ]
                  , ul [ class "error-messages" ]
                      (List.map viewProblem model.problems)
                  , viewForm model.form
                  ]
                ]
              ]
            ]
    }

viewForm : Form -> Html Msg
viewForm form =
    Htlm.form [ onSubmit SubmittedForm ]
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
                    , placeholder "Email"
                    , onInput EnteredEmail
                    , value form.email
                    ]
                    []
              ]
        , fieldset [ class "form-group" ]
              [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Email"
                    , onInput EnteredEmail
                    , value form.email
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
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign up" ]
        ]


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

-- UPDATE

type Msg
    = SubmittedForm
    | EnteredAccount String
    | EnteredName String
    | EnteredEmail String
    | EnteredPassword String
    | CompletedSignUp (Result Http.Error Viewer)
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Http.send CompletedSignUp (signUp validForm)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredAccount account ->
            updateForm (\form -> { form | account = account }) model

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedSignUp (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
                ( { model | problems = List.append mode.problems serverErrors }
                , Cmd.none
                )

        CompletedSignUp (Ok viewer) ->
            ( model
            , Viewer.store viewer)

        GotSession session ->
            ( { model | session = session }
            , Route .replaceUrl (Session.navKey session) Route.Home
            )

updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session

-- FORM

type TrimmedForm
    = Trimmed Form

type ValidatedField
    = Account
    | Name
    | Email
    | Password

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Account
    , Name
    , Email
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
    List.map (InvalidEntry field) <|
        case field of
            Account ->
                if String.isEmpty form.account then
                    [ "account can't be blank." ]
                else
                    []

            Account ->
                if String.isEmpty form.account then
                    [ "account can't be blank." ]
                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]
                else
                    []

            Name ->
                if String.isEmpty form.name then
                    [ "name can't be blank." ]
                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]
                else
                    if String.length form.password < Viewer.minPasswordChars then
                        [ "password must be at least " ++ String.fromInt Viewer.minPasswordChars ++ " characters long." ]
                    else
                        []

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { account = String.trim form.account
        , name = String.trim form.name
        , email = String.trim form.email
        , password = String.trim form.password
        }

-- HTTP

signUp : TrimmedForm -> Http.Request Viewer
signUp (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "account", Encode.string form.account )
                , ( "name", Encode.string form.name )
                , ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        body =
            user |> Http.jsonBody
    in
        Api.signUp body Viewer.decoder
