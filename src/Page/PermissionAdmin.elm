module Page.PermissionAdmin exposing (Model, Msg, init, subscriptions, toSession, update, view)

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
import Model.Permission as Permission exposing (Permission)
import Account exposing (Account)
import Viewer

-- MODEL

pageSize = 10

type alias Model =
    { session : Session
    , permissions: Status (List Permission)
    , targetPermission : Maybe Permission
    , mode : Mode
    , readMore : Bool
    , form : Form
    , searchForm : SearchForm
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

type alias SearchForm =
    { keyword : String }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

type Offset
    = Offset Int

type alias SearchParams =
    { offset: Maybe Offset
    , limit: Int
    , q : Maybe String
    }

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , permissions = Loading
      , targetPermission = Nothing
      , mode = ListMode
      , offset = Offset 0
      , readMore = True
      , form =
          { name = ""
          , description = ""
          }
      , searchForm = { keyword = "" }
      , problems = []
      }
    , Cmd.batch
        [ fetchPermissions (SearchParams (Just (Offset 0)) pageSize Nothing) session
            |> Task.attempt CompletedPermissionsLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Permission Administration"
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
          [ div [ class "col-sm-6" ]
            [ Html.form [ class "form-inline"
                        , onSubmit ClickedNewButton ]
              [ button [ class "btn btn-primary" ]
                [ text "New" ]
              ]
            ]
          , div [ class "col-sm-6" ]
              [ input
                    [ type_ "text"
                    , class "form-control"
                    , placeholder "Search..."
                    , onInput EnteredKeyword
                    , value model.searchForm.keyword ]
                    [ ]
              ]
            ]
          , div [ class "row" ] <|
            case model.permissions of
                Loading -> []
                Loaded permissions ->
                    [ table [ class "table" ]
                      [ thead []
                        [ tr []
                          [ th [] [ text "Name" ]
                          , th [] [ text "Description" ]
                          ]
                        ]
                      , tbody []
                          (List.append
                              (List.map viewPermission permissions)
                              (if model.readMore then
                                  [ tr []
                                    [ td []
                                          [ span [ custom "click"
                                                    (Decode.succeed
                                                         { stopPropagation = True
                                                         , preventDefault = True
                                                         , message = (ClickedReadMore model.offset)
                                                         }
                                                    )
                                              , href "#/permission_admin"
                                              ]
                                                [ text "Read more" ]
                                          ]
                                    ]
                                  ]
                              else
                                   [])
                          )

                      ]
                    ]
                Failed ->
                    [ Loading.error "permission" ]
        ]

viewPermission : Permission -> Html Msg
viewPermission permission =
    tr []
        [ td [ ]
          [ a [ custom "click"
                    (Decode.succeed
                         { stopPropagation = True
                         , preventDefault = True
                         , message = (ClickedUpdateLink permission.name)
                         }
                    )
              , href "#/permission_admin"
              ]
           [ text permission.name ]
          ]
        , td [ ]
            [ text permission.description ]
        ]

viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ h2 [ ] [ text "Edit Permission" ]
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

fetchPermissions: SearchParams -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess (List Permission))
fetchPermissions params session =
    let
        maybeCred =
            Session.cred session

        decoder =
            Decode.list Permission.decoder

        query = List.concat
                [ [ int "limit" params.limit ]
                , case params.offset of
                      Just (Offset offset) ->
                          [ int "offset" offset ]
                      Nothing ->
                          []
                , case  params.q of
                      Just q ->
                          [ string "q" q ]
                      Nothing ->
                          []
                ]
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["permissions"] query
            , body = Http.emptyBody
            , resolver = Api.jsonResolver decoder
            , timeout = Nothing
            }

fetchPermission: String -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess Permission)
fetchPermission name session =
    let
        maybeCred =
            Session.cred session
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["permission", name] []
            , body = Http.emptyBody
            , resolver = Api.jsonResolver Permission.decoder
            , timeout = Nothing
            }

createPermission : TrimmedForm -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess Permission)
createPermission (Trimmed form) session =
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
            , url = Api.url ["permissions" ] []
            , body = body
            , resolver = Api.jsonResolver Permission.decoder
            , timeout = Nothing
            }

updatePermission : TrimmedForm -> String -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess Permission)
updatePermission (Trimmed form) name session =
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
            , url = Api.url [ "permission", name ] []
            , body = body
            , resolver = Api.jsonResolver Permission.decoder
            , timeout = Nothing
            }

-- UPDATE

type Msg
    = GotSession Session
    | CompletedPermissionsLoad (Result Http.Error (Http.Metadata, MaybeSuccess (List Permission)))
    | CompletedPermissionLoad (Result Http.Error (Http.Metadata, MaybeSuccess Permission))
    | ClickedNewButton
    | ClickedUpdateLink String
    | ClickedReadMore Offset
    | EnteredName String
    | EnteredDescription String
    | EnteredKeyword String
    | SubmittedForm
    | CompletedSave (Result Http.Error (Http.Metadata, MaybeSuccess Permission))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedPermissionsLoad (Ok (_, res)) ->
            case res of
                Success permissions ->
                    ( { model
                          | permissions =
                              Loaded (case model.permissions of
                                          Loaded ps ->
                                              (List.append ps permissions)
                                          _ ->
                                              permissions
                                     )
                          , offset = Offset (((\(Offset o) -> o) model.offset ) + (List.length permissions))
                          , readMore = (List.length permissions) >= pageSize
                      }
                    , Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | permissions = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | problems = [ServerError "server error"] }, Cmd.none )

        CompletedPermissionsLoad (Err err) ->
            ( { model | permissions = Failed }, Cmd.none )

        CompletedPermissionLoad (Ok (_, res)) ->
            case res of
                Success permission ->
                    ( { model
                          | targetPermission = Just permission
                          , form =
                            { name = permission.name
                            , description = permission.description
                            }
                          , mode = EditMode }, Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | permissions = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | problems = [ServerError "server error"] }, Cmd.none )

        CompletedPermissionLoad (Err err) ->
            ( model, Cmd.none )

        ClickedNewButton ->
            ( { model | targetPermission = Nothing, mode = EditMode }
            , Cmd.none
            )

        ClickedUpdateLink name ->
            ( model, Task.attempt CompletedPermissionLoad ( fetchPermission name model.session ))

        ClickedReadMore offset ->
            ( model, Task.attempt CompletedPermissionsLoad ( fetchPermissions (SearchParams (Just offset) pageSize (Just model.searchForm.keyword)) model.session ))

        EnteredKeyword keyword ->
            ( { model | searchForm = { keyword = keyword }, permissions = Loading }
            , Task.attempt CompletedPermissionsLoad ( fetchPermissions (SearchParams (Just (Offset 0)) pageSize (Just keyword)) model.session ))

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedSave <|
                        case model.targetPermission of
                            Just permission ->
                                updatePermission validForm permission.name model.session
                            Nothing ->
                                createPermission validForm model.session
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        CompletedSave (Ok (_, res)) ->
            case res of
                Success permission ->
                    ( { model | mode = ListMode }
                    , fetchPermissions (SearchParams (Just model.offset) pageSize Nothing) model.session
                        |> Task.attempt CompletedPermissionsLoad)
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
