module Page.ResetPasswordChallenge exposing (Model, Msg, init, subscriptions, toSession, update, view)

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

-- MODEL

type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    , completed : Bool
    }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type alias Form =
    { account : String
    }

init : Session -> (Model, Cmd msg)
init session =
    ( { session = session
      , problems = []
      , form =
          { account = ""
          }
      , completed = False
      }

    , Cmd.none
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
                  [ h1 [ class "text-xs-center" ] [ text "Reset password" ]
                  , ul [ class "error-messages" ]
                      (List.map viewProblem model.problems)
                  , if model.completed then
                        viewCompleted
                    else
                        viewForm model.form
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
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Request for Reset Password" ]
        ]

viewCompleted : Html Msg
viewCompleted =
    div [ class "alert alert-info" ]
        [ text "Sent a reset request" ]

 -- UPDATE

type Msg
    = SubmittedForm
    | EnteredAccount String
    | CompletedResetChallenge (Result Http.Error (Http.Metadata, MaybeSuccess ()))
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedResetChallenge (resetPasswordChallenge validForm)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredAccount account ->
            updateForm (\form -> { form | account = account }) model

        CompletedResetChallenge (Err error) ->
            case error of
                _ ->
                    ( { model | problems = [ ServerError "server error"] }, Cmd.none )

        CompletedResetChallenge (Ok (_, _)) ->
            ( { model | completed = True }
            , Cmd.none )

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

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Account
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

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { account = String.trim form.account
        }

-- HTTP

resetPasswordChallenge : TrimmedForm -> Task Http.Error (Http.Metadata, MaybeSuccess ())
resetPasswordChallenge (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "account", Encode.string form.account ) ]
                    |> Http.jsonBody
    in
        Http.task
            { method = "POST"
            , headers = [ Http.header "Accept" "application/json" ]
            , url = Api.url ["password_credential", "reset_code"] []
            , body = body
            , resolver = Api.jsonResolver (Decode.succeed ())
            , timeout = Nothing
            }

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
