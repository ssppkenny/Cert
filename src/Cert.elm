module Cert exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, br, button, div, input, node, p, pre, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map3, map4, map5, string, succeed)
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
    { current : Int, review : Bool, questions : Dict Int Question }


type alias Question =
    { number : Int, text : String, code : String, type_ : QuestionType, lines : Dict String QuestionLine }


type alias JsonQuestion =
    { number : Int, type_ : String, text : String, code : String, answers : List Answer }


type alias JsonQuestions =
    List JsonQuestion


type Msg
    = GotData (Result Http.Error JsonQuestions)
    | ChangeQuestion Int
    | Checked String
    | Review


initialSearchCmd : Cmd Msg
initialSearchCmd =
    Http.get
        { url = "http://localhost:8080/questions"
        , expect = Http.expectJson GotData jsonQuestionsDecoder
        }


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
    map5 JsonQuestion
        (field "number" Json.Decode.int)
        (field "type" Json.Decode.string)
        (field "text" Json.Decode.string)
        (field "code" Json.Decode.string)
        (field "answers" answersDecoder)


jsonQuestionsDecoder : Decoder (List JsonQuestion)
jsonQuestionsDecoder =
    Json.Decode.list jsonQuestionDecoder


answersToDict : List Answer -> Dict String QuestionLine
answersToDict loa =
    let
        listOfPairs =
            List.map (\e -> ( e.letter, { text = e.text, selected = e.selected, checked = False } )) loa
    in
    Dict.fromList listOfPairs


jsonQuestionToQuestion : JsonQuestion -> Question
jsonQuestionToQuestion jq =
    { number = jq.number
    , text = jq.text
    , code = jq.code
    , type_ =
        if jq.type_ == "M" then
            Multi

        else
            Single
    , lines = answersToDict jq.answers
    }


jsonQuestionsToModel : List JsonQuestion -> Model
jsonQuestionsToModel jqs =
    let
        listOfQuestions =
            List.map jsonQuestionToQuestion jqs

        listOfPairs =
            List.map (\e -> ( e.number, e )) listOfQuestions
    in
    { current = 1, review = False, questions = Dict.fromList listOfPairs }


changeModel : String -> Model -> Model
changeModel l model =
    let
        question =
            case Dict.get model.current model.questions of
                Just q ->
                    q

                _ ->
                    { number = 0, text = "", code = "", type_ = Single, lines = Dict.fromList [] }

        newQuestionLines =
            Dict.update l (Maybe.map (\ql -> { ql | checked = not ql.checked })) question.lines

        newQuestions =
            Dict.update model.current (Maybe.map (\qu -> { qu | lines = newQuestionLines })) model.questions
    in
    { model | questions = newQuestions }


initialModel : Model
initialModel =
    { current = 1
    , review = False
    , questions =
        Dict.fromList
            [ ( 1
              , { number = 1
                , text = "This is a first question"
                , code = """<code>System.out.println("Hello")</code>"""
                , type_ = Multi
                , lines =
                    Dict.fromList
                        [ ( "A", { text = "This is a first question", selected = True, checked = False } )
                        , ( "B", { text = "This is a second question", selected = False, checked = False } )
                        ]
                }
              )
            , ( 2
              , { number = 2
                , text = "This is a second question"
                , code = "<code>import java.lang.*</code>"
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


rateModel : Model -> Float
rateModel model =
    let
        qs =
            List.map (\e -> Tuple.second e) (Dict.toList model.questions)

        s =
            List.map rateQuestion qs
    in
    toFloat (List.sum s) / toFloat (List.length s)


rateQuestion : Question -> Int
rateQuestion q =
    let
        lines =
            Dict.toList q.lines

        s =
            List.map
                (\e ->
                    if (Tuple.second e).checked == (Tuple.second e).selected then
                        1

                    else
                        0
                )
                lines

        t =
            List.sum s
    in
    if List.length lines == t then
        1

    else
        0


getQuestion : Int -> Dict Int Question -> Question
getQuestion number questions =
    let
        question =
            case Dict.get number questions of
                Nothing ->
                    { number = 1, text = "", code = "", type_ = Multi, lines = Dict.fromList [] }

                Just q ->
                    q
    in
    question


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []


view model =
    let
        question =
            getQuestion model.current model.questions

        textline =
            question.text

        code =
            question.code

        lines =
            Dict.toList question.lines
    in
    if model.current <= List.length (Dict.toList model.questions) then
        div [ class "contents" ]
            [ div [ class "questions" ]
                (List.append
                    (List.append [ div [ class "question-text" ] [ span [] [ Html.text textline ], pre [] (textHtml code) ] ]
                        (List.map
                            (\l ->
                                div
                                    (if model.review && (Tuple.second l).selected then
                                        [ class "question", class "selected" ]

                                     else
                                        [ class "question" ]
                                    )
                                    [ span [ class "letter" ] [ Html.text (Tuple.first l) ], span [] [ Html.text (Tuple.second l).text ], input [ type_ "checkbox", onClick (Checked (Tuple.first l)), checked (Tuple.second l).checked ] [], span [ class "clear" ] [] ]
                            )
                            lines
                        )
                    )
                    [ button [ type_ "button", onClick (ChangeQuestion (model.current + 1)) ] [ Html.text "Next" ], button [ type_ "button", onClick (ChangeQuestion (model.current - 1)) ] [ Html.text "Previous" ] ]
                )
            ]

    else
        div [ class "contents" ] [ div [ class "questions" ] [ div [] [ span [] [ Html.text (String.fromFloat (rateModel model)) ] ], div [] [ button [ type_ "button", onClick Review ] [ Html.text "Review Questions" ] ] ] ]


update msg model =
    case msg of
        ChangeQuestion number ->
            if number > 0 then
                ( { model | current = number }, Cmd.none )

            else
                ( model, Cmd.none )

        Checked l ->
            ( changeModel l model, Cmd.none )

        GotData (Ok jsonQuestions) ->
            ( jsonQuestionsToModel jsonQuestions, Cmd.none )

        Review ->
            ( { model | current = 1, review = True }, Cmd.none )

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
