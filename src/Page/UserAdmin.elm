module Page.UserAdmin exposing (Model, Msg, init, subscriptions, toSession, update, view)

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
import User exposing (User)
import Account exposing (Account)
import Viewer

-- MODEL

pageSize = 10

type alias Model =
    { session : Session
    , users: Status (List User)
    , targetUser : Maybe User
    , mode : Mode
    , readMore : Bool
    , form : Form
    , searchForm : SearchForm
    , offset : Offset
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
    { account : String
    , name : String
    , email : String
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
      , users = Loading
      , targetUser = Nothing
      , mode = ListMode
      , offset = Offset 0
      , readMore = True
      , form =
          { account = ""
          , name = ""
          , email = ""
          }
      , searchForm = { keyword = "" }
      , problems = []
      }
    , Cmd.batch
        [ fetchUsers (SearchParams (Just (Offset 0)) pageSize Nothing) session
            |> Task.attempt CompletedUsersLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "User Administration"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                  [ case model.mode of
                      ListMode ->
                          viewList model

                      EditMode ->
                          viewForm model.form (case model.targetUser of
                                                   Just user -> False
                                                   Nothing -> True)
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
            case model.users of
                Loading -> []
                Loaded users ->
                    [ table [ class "table" ]
                      [ thead []
                        [ tr []
                          [ th [] [ text "Account" ]
                          , th [] [ text "Name" ]
                          , th [] [ text "Email" ]
                          ]
                        ]
                      , tbody []
                          (List.append
                              (List.map viewUser users)
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
                                           , href "#/user_admin"
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
                    [ Loading.error "user" ]
        ]


viewUser : User -> Html Msg
viewUser user =
    tr []
        [ td [ ]
          [ a [ custom "click"
                    (Decode.succeed
                         { stopPropagation = True
                         , preventDefault = True
                         , message = (ClickedUpdateLink user.account)
                         }
                    )
              , href "#/user_admin"
              ]
                [ text (Account.toString user.account) ]
          ]
        , td [ ]
            [ text user.name ]
        , td [ ]
            [ text user.email ]
        ]

viewAccountField : String -> Html Msg
viewAccountField account =
    fieldset [ class "form-group" ]
        [ input
              [ class "form-control form-control-lg"
              , placeholder "Account"
              , onInput EnteredAccount
              , value account
              ]
              []
        ]

viewForm : Form -> Bool -> Html Msg
viewForm form isNew =
    Html.form [ onSubmit SubmittedForm ]
        [ h2 [ ] [ text "Edit User" ]
        , (if isNew then
               viewAccountField form.account
           else
               span [] [])
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
        , button [ class "btn btn-lg btn-primary pull-xs-right"]
            [ text "Save" ]
        ]

-- FORM

type TrimmedForm
    = Trimmed Form

type ValidatedField
    = Account
    | Name
    | Email

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Account
    , Name
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
    List.map (InvalidEntry field) <|
        case field of
            Account ->
                if String.isEmpty form.account then
                    [ "account can't be blank." ]
                else
                    []
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
        { account = String.trim form.account
        , name = String.trim form.name
        , email = String.trim form.email
        }

-- HTTP

fetchUsers: SearchParams -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess (List User))
fetchUsers params session =
    let
        maybeCred =
            Session.cred session

        decoder =
            Decode.list User.decoder

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
            , url = Api.url ["users"] query
            , body = Http.emptyBody
            , resolver = Api.jsonResolver decoder
            , timeout = Nothing
            }

fetchUser: Account -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess User)
fetchUser account session =
    let
        maybeCred =
            Session.cred session
    in
        Http.task
            { method = "GET"
            , headers = Api.headers maybeCred
            , url = Api.url ["user", (Account.toString account)] [ string "embed" "groups" ]
            , body = Http.emptyBody
            , resolver = Api.jsonResolver User.decoder
            , timeout = Nothing
            }

createUser : TrimmedForm -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess User)
createUser (Trimmed form) session =
    let
        maybeCred =
            Session.cred session

        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "email", Encode.string form.email )
                , ( "account", Encode.string form.account )
                ] |> Http.jsonBody
    in
        Http.task
            { method = "POST"
            , headers = Api.headers maybeCred
            , url = Api.url ["users" ] []
            , body = body
            , resolver = Api.jsonResolver User.decoder
            , timeout = Nothing
            }

updateUser : TrimmedForm -> Account -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess User)
updateUser (Trimmed form) account session =
    let
        maybeCred =
            Session.cred session

        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "email", Encode.string form.email )
                ] |> Http.jsonBody
    in
        Http.task
            { method = "PUT"
            , headers = Api.headers maybeCred
            , url = Api.url [ "user", (Account.toString account) ] []
            , body = body
            , resolver = Api.jsonResolver User.decoder
            , timeout = Nothing
            }

-- UPDATE

type Msg
    = GotSession Session
    | CompletedUsersLoad (Result Http.Error (Http.Metadata, MaybeSuccess (List User)))
    | CompletedUserLoad (Result Http.Error (Http.Metadata, MaybeSuccess User))
    | ClickedNewButton
    | ClickedUpdateLink Account
    | ClickedReadMore Offset
    | EnteredAccount String
    | EnteredName String
    | EnteredEmail String
    | EnteredKeyword String
    | SubmittedForm
    | CompletedSave (Result Http.Error (Http.Metadata, MaybeSuccess User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedUsersLoad (Ok (_, res)) ->
            case res of
                Success users ->
                    ( { model
                          | users =
                              Loaded (case model.users of
                                          Loaded us ->
                                              (List.append us users)
                                          _ ->
                                              users
                                     )
                          , offset = Offset (((\(Offset o) -> o) model.offset ) + (List.length users))
                          , readMore = (List.length users) >= pageSize
                      }, Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | users = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | problems = [ServerError "server error"] }, Cmd.none )

        CompletedUsersLoad (Err err) ->
            ( { model | users = Failed }, Cmd.none )

        CompletedUserLoad (Ok (_, res)) ->
            case res of
                Success user ->
                    ( { model
                          | targetUser = Just user
                          , form =
                            { account = (Account.toString user.account)
                            , name = user.name
                            , email = user.email
                            }
                          , mode = EditMode }, Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | users = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( { model | problems = [ServerError "server error"] }, Cmd.none )

        CompletedUserLoad (Err err) ->
            ( model, Cmd.none )

        ClickedNewButton ->
            ( { model | targetUser = Nothing, mode = EditMode }
            , Cmd.none
            )

        ClickedUpdateLink account ->
            ( model, Task.attempt CompletedUserLoad ( fetchUser account model.session ))

        ClickedReadMore offset ->
            ( model, Task.attempt CompletedUsersLoad ( fetchUsers (SearchParams (Just offset) pageSize (Just model.searchForm.keyword)) model.session ))

        EnteredKeyword keyword ->
            ( { model | searchForm = { keyword = keyword }, users = Loading }
            , Task.attempt CompletedUsersLoad ( fetchUsers (SearchParams (Just (Offset 0)) pageSize (Just keyword)) model.session ))

        EnteredAccount account ->
            updateForm (\form -> { form | account = account }) model

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Task.attempt CompletedSave <|
                        case model.targetUser of
                            Just user ->
                                updateUser validForm user.account model.session
                            Nothing ->
                                createUser validForm model.session
                    )
                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        CompletedSave (Ok (_, res)) ->
            case res of
                Success role ->
                    ( { model | mode = ListMode, users = Loaded [] }
                    , fetchUsers (SearchParams (Just (Offset 0)) pageSize Nothing) model.session
                        |> Task.attempt CompletedUsersLoad)
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
