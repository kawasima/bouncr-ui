module Page.Profile exposing (Model, Msg, decoder, init, update, viewProfiles)

import Api exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Url exposing (Url)

-- MODEL

type alias Form =
    { account : String
    , name : String
    , email : String
    , password : String
    }

type alias SignUpResponse =
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
    { title = "Sign Up"
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
    Html.form [ onSubmit SubmittedForm ]
        [ fieldset [ class "form-group" ]
              [ text form.account ]
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
            [ text "Update Profiles" ]
        ]
