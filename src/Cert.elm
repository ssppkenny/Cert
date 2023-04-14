module Cert exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map3, map4, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode exposing (string)


type QuestionType
    = Multi
    | Single


type alias QuestionLine =
    { text : String, selected : Bool, checked : Bool }


type alias Answer =
    { questionNumner : Int, letter : String, text : String, selected : Bool }


type alias Model =
    { current : Int, questions : Dict Int Question }


type alias Question =
    { text : String, type_ : QuestionType, lines : Dict String QuestionLine }


type alias JsonQuestion =
    { number : Int, type_ : String, text : String, answers : List Answer }


type alias JsonQuestions =
    List JsonQuestion


type Msg
    = GotData (Result Http.Error JsonQuestions)
    | ChangeQuestion Int


initialSearchCmd : Cmd Msg
initialSearchCmd =
    Http.get
        { url = "http://localhost:8080/questions"
        , expect = Http.expectJson GotData jsonQuestionsDecoder
        }


buildModel : Int -> Dict Int Question -> Model
buildModel n d =
    { current = n, questions = d }


answerDecoder : Decoder Answer
answerDecoder =
    map4 Answer
        (field "questionNumber" Json.Decode.int)
        (field "letter" Json.Decode.string)
        (field "text" Json.Decode.string)
        (field "selected" bool)


answersDecoder : Decoder (List Answer)
answersDecoder =
    Json.Decode.list answerDecoder


jsonQuestionDecoder : Decoder JsonQuestion
jsonQuestionDecoder =
    map4 JsonQuestion
        (field "number" Json.Decode.int)
        (field "type" Json.Decode.string)
        (field "text" Json.Decode.string)
        (field "answers" answersDecoder)


jsonQuestionsDecoder : Decoder (List JsonQuestion)
jsonQuestionsDecoder =
    Json.Decode.list jsonQuestionDecoder


initialModel : Model
initialModel =
    { current = 1
    , questions =
        Dict.fromList
            [ ( 1
              , { text = "This is a first question"
                , type_ = Multi
                , lines =
                    Dict.fromList
                        [ ( "A", { text = "This is a first question", selected = True, checked = False } )
                        , ( "B", { text = "This is a second question", selected = False, checked = False } )
                        ]
                }
              )
            , ( 2
              , { text = "This is a second question"
                , type_ = Single
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
                    { text = "", type_ = Multi, lines = Dict.fromList [] }

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
