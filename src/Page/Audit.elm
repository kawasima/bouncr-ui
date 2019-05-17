module Page.Audit exposing (Model, Msg, init, subscriptions, toSession, update, view)

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
import Model.UserAction as UserAction exposing (UserAction)
import Viewer
import Iso8601

-- MODEL

pageSize = 10

type alias Model =
    { session : Session
    , userActions: Status (List UserAction)
    , readMore : Bool
    , searchForm : SearchForm
    , offset: Offset
    }

type Status a
    = Loading
    | Loaded a
    | Failed

type alias SearchForm =
    { keyword : String }

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
      , userActions = Loading
      , offset = Offset 0
      , readMore = True
      , searchForm = { keyword = "" }
      }
    , Cmd.batch
        [ fetchUserActions (SearchParams (Just (Offset 0)) pageSize Nothing) session
            |> Task.attempt CompletedUserActionsLoad
        ]
    )

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Audit user actions"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                  [ viewList model ]
            ]
    }
viewList : Model -> Html Msg
viewList model =
    div []
        [ div [ class "row" ]
          [ div [ class "col-sm-6" ]
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
            case model.userActions of
                Loading -> []
                Loaded userActions ->
                    [ table [ class "table" ]
                      [ thead []
                        [ tr []
                          [ th [] [ text "Time" ]
                          , th [] [ text "Actor" ]
                          , th [] [ text "Action Type" ]
                          , th [] [ text "Details" ]
                          ]
                        ]
                      , tbody []
                          (List.append
                              (List.map viewUserAction userActions)
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
                                           , href "#/audit"
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
                    [ Loading.error "userAction" ]
        ]

viewUserAction : UserAction -> Html Msg
viewUserAction userAction =
    tr []
        [ td [ ]
            [ text (Iso8601.fromTime userAction.createdAt) ]
        , td [ ]
            [ text userAction.actor ]
        , td [ ]
            [ text userAction.actionType ]
        , td [ ]
            [ text (case userAction.options of
                        Just options -> options
                        Nothing -> "") ]
        ]

-- HTTP

fetchUserActions: SearchParams -> Session -> Task Http.Error (Http.Metadata, MaybeSuccess (List UserAction))
fetchUserActions params session =
    let
        maybeCred =
            Session.cred session

        decoder =
            Decode.list UserAction.decoder

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
            , url = Api.url ["actions"] query
            , body = Http.emptyBody
            , resolver = Api.jsonResolver decoder
            , timeout = Nothing
            }

-- UPDATE

type Msg
    = GotSession Session
    | CompletedUserActionsLoad (Result Http.Error (Http.Metadata, MaybeSuccess (List UserAction)))
    | ClickedReadMore Offset
    | EnteredKeyword String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedUserActionsLoad (Ok (_, res)) ->
            case res of
                Success userActions ->
                    ( { model
                          | userActions =
                              Loaded (case model.userActions of
                                          Loaded uas ->
                                              (List.append uas userActions)
                                          _ ->
                                              userActions
                                     )
                          , offset = Offset (((\(Offset o) -> o) model.offset ) + (List.length userActions))
                          , readMore = (List.length userActions) >= pageSize
                      }
                    , Cmd.none )
                Failure problem ->
                    case problem.status of
                        401 ->
                            ( { model | userActions = Failed }
                            , Route.replaceUrl (Session.navKey model.session) Route.SignIn)
                        _ ->
                            ( model, Cmd.none )

        CompletedUserActionsLoad (Err err) ->
            ( { model | userActions = Failed }, Cmd.none )

        ClickedReadMore offset ->
            ( model, Task.attempt CompletedUserActionsLoad ( fetchUserActions (SearchParams (Just offset) pageSize (Just model.searchForm.keyword)) model.session ))

        EnteredKeyword keyword ->
            ( { model | searchForm = { keyword = keyword }, userActions = Loading }
            , Task.attempt CompletedUserActionsLoad ( fetchUserActions (SearchParams (Just (Offset 0)) pageSize (Just keyword)) model.session ))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
