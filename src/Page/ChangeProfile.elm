module Page.ChangeProfile exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url exposing (Url)
import User exposing (User)
import Account exposing (Account)
import Viewer

-- MODEL

type alias Model =
    { session : Session
    , problems : List Problem
    , user : Status User
    , form : Form
    }

type Status a
    = Loading
    | Loaded a
    | Failed

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type alias Form =
    { name : String
    , email : String
    }

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , user = Loading
      , problems = []
      , form =
          { name = ""
          , email = ""
          }
      }
    , Cmd.batch
        [ fetchUser session
        |> Task.attempt CompletedUserLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Change Profile"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
              [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                  [ h1 [ class "text-xs-center" ] [ text "Change Profile" ]
                  , p [ class "text-cs-center" ]
                      [ a [ Route.href Route.SignIn ]
                        [ text "Hav an account?" ]
                      ]
                  , ul [ class "error-messages" ]
                      (List.map viewProblem model.problems)
                  , viewForm model.user model.form
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

viewForm : Status User -> Form -> Html Msg
viewForm user form =
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
              [ text (case user of
                          Loading -> ""
                          Loaded u -> Account.toString u.account
                          Failed -> ""
                     )]
        , fieldset [ class "form-group" ]
              [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Name"
                    , onInput EnteredName
                    , value form.name
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
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Update Profiles" ]
        ]

-- UPDATE

type Msg
    = SubmittedForm
    | EnteredName String
    | EnteredEmail String
    | CompletedUserLoad (Result Http.Error (Http.Metadata, MaybeSuccess User))
    | CompletedChange (Result Http.Error (Http.Metadata, MaybeSuccess ()))
    | GotSession Session

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedChange (changeProfile validForm model.session)
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        CompletedUserLoad (Ok (_, res)) ->
            case res of
                Success user ->
                    ( { model | user = Loaded user, form = { name = user.name, email = user.email } }, Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | user = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( model, Cmd.none )

        CompletedUserLoad (Err err) ->
            case err of
                Http.BadStatus status ->
                    case status of
                        401 ->
                            ( { model | user = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | user = Failed }, Cmd.none )
                _ ->
                    ( { model | user = Failed }, Cmd.none )

        CompletedChange (Err error) ->
            case error of
                _ ->
                    ( { model | problems = [] }, Cmd.none )

        CompletedChange (Ok (_, _)) ->
            ( model
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

-- SERIALIZATION

-- FORM

type TrimmedForm
    = Trimmed Form

type ValidatedField
    = Name
    | Email

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Name
    , Email
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
            Name ->
                if String.isEmpty form.name then
                    [ "name can't be blank." ]
                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]
                else
                    []

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { name = String.trim form.name
        , email = String.trim form.email
        }

-- HTTP

fetchUser: Session -> Task Http.Error (Http.Metadata, MaybeSuccess User)
fetchUser session =
    let
        maybeCred =
            Session.cred session

        maybeViewer =
            Session.viewer session
    in
        case maybeViewer of
            Just viewer ->
                Http.task
                    { method = "GET"
                    , headers = Api.headers maybeCred
                    , url = Api.url
                            ["user", Account.toString (Viewer.account viewer)]
                            [ ]
                    , body = Http.emptyBody
                    , resolver = Api.jsonResolver User.decoder
                    , timeout = Nothing
                    }
            Nothing ->
                Task.fail (Http.BadStatus 401)

changeProfile : TrimmedForm -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess ())
changeProfile (Trimmed form) session =
    let
        maybeCred =
            Session.cred session

        account =
           case Session.viewer session of
               Just viewer ->
                   Viewer.account viewer
               Nothing ->
                   Account.anonymous

        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "email", Encode.string form.email )
                ]
                |> Http.jsonBody
    in
        Http.task
            { method = "PUT"
            , headers = Api.headers maybeCred
            , url = Api.url ["user", Account.toString account] []
            , body = body
            , resolver = Api.jsonResolver (Decode.succeed ())
            , timeout = Nothing
            }

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
