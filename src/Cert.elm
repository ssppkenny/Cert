module Cert exposing (main)

import Dict exposing (Dict)
import Html exposing (button, div, input, span, text)
import Html.Attributes exposing (..)


type alias QuestionLine =
    { text : String, selected : Bool, checked : Bool }


type alias Model =
    { current : Int, questions : Dict Int Question }


type alias Question =
    { text : String, lines : Dict String QuestionLine }


initialModel : Model
initialModel =
    { current = 1
    , questions =
        Dict.fromList
            [ ( 1
              , { text = "This is a question"
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
    div [ class "contents" ] [ div [ class "questions" ] (List.append (List.append [ div [ class "question-text" ] [ span [] [ text textline ] ] ] (List.map (\l -> div [ class "question" ] [ span [] [ text (Tuple.first l) ], span [] [ text (Tuple.second l).text ], input [ type_ "checkbox" ] [], span [ class "clear" ] [] ]) lines)) [ button [ type_ "button" ] [ text "Next" ] ]) ]


update msg model =
    model


main =
    view initialModel
