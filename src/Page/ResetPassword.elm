module Page.ResetPassword exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
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
import Task exposing (Task)
import Viewer exposing (Viewer)

import Model.InitialPassword as InitialPassword exposing (InitialPassword)

-- MODEL

type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    , initialPassword: String
    }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type alias Form =
    { account : String
    , password : String
    }

init : String -> Session -> (Model, Cmd Msg)
init code session =
    ( { session = session
      , problems = []
      , form =
          { account = ""
          , password = ""
          }
      , initialPassword = ""
      }
    , Cmd.batch
        [ (resetPassword code)
        |> Task.attempt CompletedReset
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Reset password"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
              [ div [ class "row" ]
                [ div [class "col-md-6 offset-md-3 col-xs-12" ]
                  [ h1 [ class "text-xs-center" ] [ text "Change password" ]
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
                  , placeholder "New Password"
                  , onInput EnteredPassword
                  , value form.password
                  ]
                  []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Change Password" ]
        ]

 -- UPDATE

type Msg
    = SubmittedForm
    | EnteredAccount String
    | EnteredPassword String
    | CompletedReset (Result Http.Error (Http.Metadata, MaybeSuccess InitialPassword))
    | CompletedChange (Result Http.Error (Http.Metadata, MaybeSuccess ()))
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedChange (changePassword model.initialPassword validForm)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )
        EnteredAccount account ->
            updateForm (\form -> { form | account = account }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedReset (Err error) ->
            case error of
                _ ->
                    ( { model | problems = [ ServerError "server error"] }, Cmd.none )

        CompletedReset (Ok (_, maybeInitialPassword)) ->
            case maybeInitialPassword of
                Success initialPassword ->
                    ( { model | initialPassword = initialPassword.password }
                    , Cmd.none )
                Failure problem ->
                    ( { model | problems = [ ServerError "server error"] }
                    , Cmd.none )

        CompletedChange (Err error) ->
            case error of
                _ ->
                    ( { model | problems = [ ServerError "server error"] }, Cmd.none )

        CompletedChange (Ok (_, _)) ->
            ( model
            , Route.replaceUrl (Session.navKey model.session) Route.SignIn )

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
        { account  = String.trim form.account
        , password = String.trim form.password
        }

-- HTTP

resetPassword : String -> Task Http.Error (Http.Metadata, MaybeSuccess InitialPassword)
resetPassword code =
    let
        body =
            Encode.object
                [ ( "code", Encode.string code ) ]
                    |> Http.jsonBody
    in
        Http.task
            { method = "PUT"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = Api.url ["password_credential", "reset"] []
            , body = body
            , resolver = Api.jsonResolver InitialPassword.decoder
            , timeout = Nothing
            }

changePassword : String -> TrimmedForm -> Task Http.Error (Http.Metadata, MaybeSuccess ())
changePassword initialPassword (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "account", Encode.string form.account )
                , ( "old_password", Encode.string initialPassword )
                , ( "new_password", Encode.string form.password )
                ]
                |> Http.jsonBody
    in
        Http.task
            { method = "PUT"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = Api.url ["password_credential"] []
            , body = body
            , resolver = Api.jsonResolver (Decode.succeed ())
            , timeout = Nothing
            }

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
