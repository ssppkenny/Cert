module Cert exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Encode exposing (string)


type alias QuestionLine =
    { text : String, selected : Bool, checked : Bool }


type alias Model =
    { current : Int, questions : Dict Int Question }


type alias Question =
    { text : String, lines : Dict String QuestionLine }


type Msg
    = GotData (Result Http.Error String)
    | ChangeQuestion Int


initialSearchCmd : Cmd Msg
initialSearchCmd =
    Cmd.none


initialModel : Model
initialModel =
    { current = 1
    , questions =
        Dict.fromList
            [ ( 1
              , { text = "This is a first question"
                , lines =
                    Dict.fromList
                        [ ( "A", { text = "This is a first question", selected = True, checked = False } )
                        , ( "B", { text = "This is a second question", selected = False, checked = False } )
                        ]
                }
              )
            , ( 2
              , { text = "This is a second question"
                , lines =
                    Dict.fromList
                        [ ( "A", { text = "This is a first question", selected = True, checked = False } )
                        , ( "B", { text = "This is a second question", selected = False, checked = False } )
                        ]
                }
              )
            ]
    }


getQuestion : Int -> Dict Int Question -> Question
getQuestion number questions =
    let
        question =
            case Dict.get number questions of
                Nothing ->
                    { text = "", lines = Dict.fromList [] }

                Just q ->
                    q
    in
    question


view model =
    let
        current =
            model.current

        question =
            getQuestion current model.questions

        textline =
            question.text

        lines =
            Dict.toList question.lines
    in
    div [ class "contents" ] [ div [ class "questions" ] (List.append (List.append [ div [ class "question-text" ] [ span [] [ text textline ] ] ] (List.map (\l -> div [ class "question" ] [ span [ class "letter" ] [ text (Tuple.first l) ], span [] [ text (Tuple.second l).text ], input [ type_ "checkbox" ] [], span [ class "clear" ] [] ]) lines)) [ button [ type_ "button", onClick (ChangeQuestion (model.current + 1)) ] [ text "Next" ] ]) ]


update msg model =
    case msg of
        ChangeQuestion number ->
            ( { model | current = number }, Cmd.none )

        _ ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialSearchCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
