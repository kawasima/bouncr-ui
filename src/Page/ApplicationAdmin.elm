module Page.ApplicationAdmin exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred, MaybeSuccess(..))
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Loading
import Page
import Session exposing (Session)
import Route
import Task exposing (Task)
import Time
import Url.Builder exposing (int, string)
import Model.Application as Application exposing (Application)
import Account exposing (Account)
import Viewer

-- MODEL

pageSize = 10

type alias Model =
    { session : Session
    , applications: Status (List Application)
    , targetApplication : Maybe Application
    , mode : Mode
    , form : Form
    , offset: Offset
    , problems : List Problem
    }

type Mode
    = ListMode
    | EditMode

type Status a
    = Loading
    | Loaded a
    | Failed

type alias Form =
    { name : String
    , description : String
    }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type Offset
    = Offset Int

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , applications = Loading
      , targetApplication = Nothing
      , mode = ListMode
      , offset = Offset 0
      , form =
          { name = ""
          , description = ""
          }
      , problems = []
      }
    , Cmd.batch
        [ fetchApplications (Offset 0) session
            |> Task.attempt CompletedApplicationsLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Application Administration"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                  [ case model.mode of
                      ListMode ->
                          viewList model

                      EditMode ->
                          viewForm model.form
                  ]
            ]
    }

viewList : Model -> Html Msg
viewList model =
    div []
        [ div [ class "row" ]
              [ button
                    [ class "btn btn-primary"
                    , onClick ClickedNewButton
                    ]
                    [ text "New" ] ]
        , div [ class "row" ] <|
            case model.applications of
                Loading -> []
                Loaded applications ->
                    [ table [ class "table" ]
                      [ thead []
                        [ tr []
                          [ th [] [ text "Name" ]
                          , th [] [ text "Description" ]
                          , th [] [ text "Pass to" ]
                          ]
                        ]
                      , tbody []
                          (List.append
                              (List.map viewApplication applications)
                              [ tr []
                                    [ td []
                                          [ a [ custom "click"
                                                    (Decode.succeed
                                                         { stopPropagation = False
                                                         , preventDefault = True
                                                         , message = (ClickedReadMore model.offset)
                                                         }
                                                    )
                                              , href "#/application_admin"
                                              ]
                                                [ text "Read more" ]
                                          ]
                                    ]
                              ]
                          )

                      ]
                    ]
                Failed ->
                    [ Loading.error "application" ]
        ]

viewApplication : Application -> Html Msg
viewApplication application =
    tr []
        [ td [ ]
          [ a [ custom "click"
                    (Decode.succeed
                         { stopPropagation = True
                         , preventDefault = True
                         , message = (ClickedUpdateLink application.name)
                         }
                    )
              , href "#/application_admin"
              ]
           [ text application.name ]
          ]
        , td [ ]
            [ text application.description ]
        , td [ ]
            [ text application.pass_to ]
        ]

viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ h2 [ ] [ text "Edit Application" ]
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
                    , placeholder "Description"
                    , onInput EnteredDescription
                    , value form.description
                    ]
                    []
              ]
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Save" ]
        ]

-- FORM

type TrimmedForm
    = Trimmed Form

type ValidatedField
    = Name
    | Description

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Name
    , Description
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
            Name ->
                if String.isEmpty form.name then
                    [ "name can't be blank." ]
                else
                    []
            Description ->
                if String.isEmpty form.description then
                    [ "description can't be blank." ]
                else
                    []

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { name = String.trim form.name
        , description = String.trim form.description
        }

-- HTTP

fetchApplications: Offset -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess (List Application))
fetchApplications (Offset offset) session =
    let
        maybeCred =
            Session.cred session

        decoder =
            Decode.list Application.decoder
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["applications"] [ int "offset" offset, string "embed" "(realms)" ]
            , body = Http.emptyBody
            , resolver = Api.jsonResolver decoder
            , timeout = Nothing
            }

fetchApplication: String -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess Application)
fetchApplication name session =
    let
        maybeCred =
            Session.cred session
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["application", name] []
            , body = Http.emptyBody
            , resolver = Api.jsonResolver Application.decoder
            , timeout = Nothing
            }

createApplication : TrimmedForm -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess Application)
createApplication (Trimmed form) session =
    let
        maybeCred =
            Session.cred session

        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "description", Encode.string form.description )
                ] |> Http.jsonBody
    in
        Http.task
            { method = "POST"
            , headers = Api.headers maybeCred
            , url = Api.url ["applications" ] []
            , body = body
            , resolver = Api.jsonResolver Application.decoder
            , timeout = Nothing
            }

updateApplication : TrimmedForm -> String -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess Application)
updateApplication (Trimmed form) name session =
    let
        maybeCred =
            Session.cred session

        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "description", Encode.string form.description )
                ] |> Http.jsonBody

    in
        Http.task
            { method = "PUT"
            , headers = Api.headers maybeCred
            , url = Api.url [ "application", name ] []
            , body = body
            , resolver = Api.jsonResolver Application.decoder
            , timeout = Nothing
            }

-- UPDATE

type Msg
    = GotSession Session
    | CompletedApplicationsLoad (Result Http.Error (Http.Metadata, MaybeSuccess (List Application)))
    | CompletedApplicationLoad (Result Http.Error (Http.Metadata, MaybeSuccess Application))
    | ClickedNewButton
    | ClickedUpdateLink String
    | ClickedReadMore Offset
    | EnteredName String
    | EnteredDescription String
    | SubmittedForm
    | CompletedSave (Result Http.Error (Http.Metadata, MaybeSuccess Application))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedApplicationsLoad (Ok (_, res)) ->
            case res of
                Success applications ->
                    ( { model
                          | applications =
                              Loaded (case model.applications of
                                          Loaded ps ->
                                              (List.append ps applications)
                                          _ ->
                                              applications
                                     )
                          , offset = Offset (((\(Offset o) -> o) model.offset ) + (List.length applications))
                      }
                    , Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | applications = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | problems = [ServerError "server error"] }, Cmd.none )

        CompletedApplicationsLoad (Err err) ->
            ( { model | applications = Failed }, Cmd.none )

        CompletedApplicationLoad (Ok (_, res)) ->
            case res of
                Success application ->
                    ( { model
                          | targetApplication = Just application
                          , form =
                            { name = application.name
                            , description = application.description
                            }
                          , mode = EditMode }, Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | applications = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | problems = [ServerError "server error"] }, Cmd.none )

        CompletedApplicationLoad (Err err) ->
            ( model, Cmd.none )

        ClickedNewButton ->
            ( { model | targetApplication = Nothing, mode = EditMode }
            , Cmd.none
            )

        ClickedUpdateLink name ->
            ( model, Task.attempt CompletedApplicationLoad ( fetchApplication name model.session ))

        ClickedReadMore offset ->
            ( model, Task.attempt CompletedApplicationsLoad ( fetchApplications offset model.session ))

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedSave <|
                        case model.targetApplication of
                            Just application ->
                                updateApplication validForm application.name model.session
                            Nothing ->
                                createApplication validForm model.session
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        CompletedSave (Ok (_, res)) ->
            case res of
                Success application ->
                    ( { model | mode = ListMode }
                    , fetchApplications model.offset model.session
                        |> Task.attempt CompletedApplicationsLoad)
                Failure problem ->
                    ( { model | problems = [ ServerError "server error" ] }, Cmd.none )

        CompletedSave (Err error) ->
            ( { model | problems = [ ServerError "server error" ] }, Cmd.none )


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
