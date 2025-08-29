module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Http
import Json.Decode as Decode
import Random
import Time
import Task
import Browser.Dom as Dom


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { meditations : List Meditation
    , currentMeditation : Maybe Meditation
    , userInput : String
    , currentPosition : Int
    , isComplete : Bool
    , mistakes : Int
    , correctedPositions : List Int
    , startTime : Maybe Time.Posix
    , currentTime : Time.Posix
    , endTime : Maybe Time.Posix
    }


type alias Meditation =
    { id : String
    , text : String
    , author : String
    , packId : String
    , source : String
    }


type Msg
    = LoadMeditations (Result Http.Error (List Meditation))
    | SelectRandomMeditation Int
    | KeyPressed String
    | StartOver
    | GotRandomIndex Int
    | Tick Time.Posix
    | FocusTypingArea


init : () -> ( Model, Cmd Msg )
init _ =
    ( { meditations = []
      , currentMeditation = Nothing
      , userInput = ""
      , currentPosition = 0
      , isComplete = False
      , mistakes = 0
      , correctedPositions = []
      , startTime = Nothing
      , currentTime = Time.millisToPosix 0
      , endTime = Nothing
      }
    , loadMeditations
    )


loadMeditations : Cmd Msg
loadMeditations =
    Http.get
        { url = "./meditations.json"
        , expect = Http.expectJson LoadMeditations meditationsDecoder
        }


meditationsDecoder : Decode.Decoder (List Meditation)
meditationsDecoder =
    Decode.list meditationDecoder


meditationDecoder : Decode.Decoder Meditation
meditationDecoder =
    Decode.map5 Meditation
        (Decode.field "id" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "packId" Decode.string)
        (Decode.field "source" Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadMeditations (Ok meditations) ->
            let
                filteredMeditations =
                    List.filter (\m -> String.length m.text <= 300) meditations
            in
            ( { model 
                | meditations = filteredMeditations
              }
            , Random.generate GotRandomIndex (Random.int 0 (List.length filteredMeditations - 1))
            )

        LoadMeditations (Err _) ->
            ( model, Cmd.none )

        GotRandomIndex index ->
            let
                selectedMeditation =
                    List.drop index model.meditations |> List.head
            in
            ( { model | currentMeditation = selectedMeditation }
            , Task.attempt (\_ -> FocusTypingArea) (Dom.focus "typing-area")
            )

        FocusTypingArea ->
            ( model, Cmd.none )

        SelectRandomMeditation _ ->
            ( model
            , Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1))
            )

        KeyPressed key ->
            case model.currentMeditation of
                Nothing ->
                    ( model, Cmd.none )

                Just meditation ->
                    if model.isComplete && key == "Enter" then
                        -- Start next text when Enter is pressed after completion
                        ( { model
                            | userInput = ""
                            , currentPosition = 0
                            , isComplete = False
                            , mistakes = 0
                            , correctedPositions = []
                            , startTime = Nothing
                            , endTime = Nothing
                          }
                        , Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1))
                        )
                    else if model.isComplete then
                        -- Ignore other keys when completed
                        ( model, Cmd.none )
                    else if isModifierKey key then
                        -- Ignore modifier keys
                        ( model, Cmd.none )
                    else if key == "Backspace" then
                        -- Ignore backspace key completely
                        ( model, Cmd.none )
                    else
                        let
                            targetChar =
                                String.slice model.currentPosition (model.currentPosition + 1) meditation.text

                            isCorrect =
                                String.toLower key == String.toLower targetChar

                            isFirstCharacter =
                                model.currentPosition == 0

                            -- Handle space character logic
                            result =
                                if targetChar == " " && key == " " then
                                    -- Direct space input - treat as correct
                                    { userInput = model.userInput ++ " "
                                    , position = model.currentPosition + 1
                                    , mistakes = model.mistakes
                                    , correctedPositions = model.correctedPositions
                                    }
                                else if targetChar == " " && key /= " " then
                                    -- Non-space input when space expected - auto-insert space and check next character
                                    let
                                        nextTargetChar =
                                            String.slice (model.currentPosition + 1) (model.currentPosition + 2) meditation.text
                                        
                                        nextIsCorrect =
                                            String.toLower key == String.toLower nextTargetChar
                                        
                                        wasAlreadyIncorrect =
                                            List.member (model.currentPosition + 1) model.correctedPositions
                                        
                                        isNewMistake =
                                            not nextIsCorrect && not wasAlreadyIncorrect
                                        
                                        newCorrectedPositions =
                                            if isNewMistake then
                                                (model.currentPosition + 1) :: model.correctedPositions
                                            else
                                                model.correctedPositions
                                        
                                        newMistakes =
                                            if isNewMistake then
                                                model.mistakes + 1
                                            else
                                                model.mistakes
                                        
                                        newPosition =
                                            if nextIsCorrect then
                                                model.currentPosition + 2  -- Skip space and advance to next
                                            else
                                                model.currentPosition + 1  -- Stay at next character position
                                        
                                        newInput =
                                            if nextIsCorrect then
                                                model.userInput ++ " " ++ key
                                            else
                                                model.userInput ++ " "
                                    in
                                    { userInput = newInput
                                    , position = newPosition
                                    , mistakes = newMistakes
                                    , correctedPositions = newCorrectedPositions
                                    }
                                else
                                    -- Normal character handling
                                    let
                                        wasAlreadyIncorrect =
                                            List.member model.currentPosition model.correctedPositions

                                        isNewMistake =
                                            not isCorrect && not wasAlreadyIncorrect && not isFirstCharacter

                                        newCorrectedPositions =
                                            if isNewMistake then
                                                model.currentPosition :: model.correctedPositions
                                            else
                                                model.correctedPositions

                                        newUserInput =
                                            if isCorrect then
                                                model.userInput ++ key
                                            else
                                                model.userInput

                                        newPosition =
                                            if isCorrect then
                                                model.currentPosition + 1
                                            else
                                                model.currentPosition

                                        newMistakes =
                                            if isNewMistake then
                                                model.mistakes + 1
                                            else
                                                model.mistakes
                                    in
                                    { userInput = newUserInput
                                    , position = newPosition
                                    , mistakes = newMistakes
                                    , correctedPositions = newCorrectedPositions
                                    }

                            mistakeLimitExceeded =
                                result.mistakes >= (calculateMaxLives model meditation.text)

                            isComplete =
                                result.position >= String.length meditation.text

                            newStartTime =
                                case model.startTime of
                                    Nothing ->
                                        if result.position > model.currentPosition then
                                            Just model.currentTime
                                        else
                                            Nothing
                                    Just time ->
                                        Just time

                            newEndTime =
                                if isComplete then
                                    Just model.currentTime
                                else
                                    model.endTime
                        in
                        if mistakeLimitExceeded then
                            -- Reset when mistake limit exceeded
                            ( { model
                                | userInput = ""
                                , currentPosition = 0
                                , mistakes = 0
                                , correctedPositions = []
                                , startTime = Nothing
                                , endTime = Nothing
                              }
                            , Cmd.none
                            )
                        else
                            ( { model
                                | userInput = result.userInput
                                , currentPosition = result.position
                                , mistakes = result.mistakes
                                , isComplete = isComplete
                                , correctedPositions = result.correctedPositions
                                , startTime = newStartTime
                                , endTime = newEndTime
                              }
                            , Cmd.none
                            )

        StartOver ->
            ( { model
                | userInput = ""
                , currentPosition = 0
                , isComplete = False
                , mistakes = 0
                , correctedPositions = []
                , startTime = Nothing
                , endTime = Nothing
              }
            , Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1))
            )

        Tick time ->
            ( { model | currentTime = time }, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick


isModifierKey : String -> Bool
isModifierKey key =
    List.member key
        [ "Shift", "Control", "Alt", "Meta", "CapsLock", "Tab"
        , "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight"
        , "Home", "End", "PageUp", "PageDown"
        , "Insert", "Delete", "Escape", "F1", "F2", "F3", "F4", "F5"
        , "F6", "F7", "F8", "F9", "F10", "F11", "F12"
        ]


calculateAccuracy : Model -> String -> Int
calculateAccuracy model targetText =
    let
        totalChars = model.currentPosition
        correctChars = totalChars - List.length model.correctedPositions
    in
    if totalChars > 0 then
        round ((toFloat correctChars / toFloat totalChars) * 100)
    else
        100


calculateWPM : Model -> String -> Int
calculateWPM model targetText =
    case model.startTime of
        Nothing ->
            0
        Just startTime ->
            let
                endTime = 
                    case model.endTime of
                        Just time -> time
                        Nothing -> model.currentTime
                        
                elapsedTime = Time.posixToMillis endTime - Time.posixToMillis startTime
                elapsedMinutes = toFloat elapsedTime / 60000
                typedChars = model.currentPosition
                wordsTyped = toFloat typedChars / 5
            in
            if elapsedMinutes > 0 then
                round (wordsTyped / elapsedMinutes)
            else
                0


calculateMaxLives : Model -> String -> Int
calculateMaxLives model targetText =
    let
        wordCount = 
            targetText
                |> String.words
                |> List.length
    in
    Basics.max 1 (wordCount // 15)


viewLives : Model -> String -> List (Html Msg)
viewLives model targetText =
    let
        maxLives = calculateMaxLives model targetText
        remainingLives = Basics.max 0 (maxLives - model.mistakes)
    in
    List.range 1 maxLives
        |> List.map (\index ->
            if index <= remainingLives then
                span [ class "life-heart full" ] [ text "❤️" ]
            else
                span [ class "life-heart empty" ] [ text "🤍" ]
           )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title" ] [ text "Daily Typing Practice" ]
        , h2 [ class "subtitle" ] [ text "명상록으로 하루를 시작하세요" ]
        , case model.currentMeditation of
            Nothing ->
                div [ class "loading" ] [ text "명상록을 불러오는 중..." ]
            
            Just meditation ->
                viewTypingPractice model meditation
        ]



viewTypingPractice : Model -> Meditation -> Html Msg
viewTypingPractice model meditation =
    div [ class "practice-area" ]
        [ div [ class "meditation-info" ]
            [ h3 [] [ text meditation.author ]
            , p [ class "source" ] [ text meditation.source ]
            ]
        , div [ class "typing-area" ]
            [ div 
                [ class "target-text"
                , tabindex 0
                , id "typing-area"
                , on "keydown" (Json.map KeyPressed (Json.field "key" Json.string))
                ]
                [ viewTypingText meditation.text model.currentPosition model.correctedPositions ]
            ]
        , div [ class "stats" ]
            [ div [ class "lives" ]
                [ div [ class "lives-display" ] (viewLives model meditation.text)
                , if model.mistakes >= (calculateMaxLives model meditation.text) then
                    div [ class "lives-warning" ]
                        [ text "⚠️ 생명이 모두 소진되었습니다! 다음 오타 시 처음부터 다시 시작됩니다." ]
                  else
                    text ""
                ]
            ]
        , if model.isComplete then
            div [ class "completion" ]
                [ h3 [] [ text "완료!" ]
                , p [] [ text "명상록 한 구절을 완성했습니다." ]
                , p [ class "enter-hint" ] [ text "Enter 키를 눌러 다음 구절로 이동하세요" ]
                , div [ class "final-stats" ]
                    [ div [ class "final-wpm" ]
                        [ text ("타자 속도: " ++ String.fromInt (calculateWPM model meditation.text) ++ " WPM") ]
                    , div [ class "final-accuracy" ]
                        [ text ("정확도: " ++ String.fromInt (calculateAccuracy model meditation.text) ++ "%") ]
                    ]
                , button [ onClick StartOver, class "btn-primary" ] [ text "새로운 구절로 시작" ]
                ]
          else
            div [ class "controls" ]
                [ button [ onClick StartOver, class "btn-secondary" ] [ text "다시 시작" ] ]
        ]



viewTypingText : String -> Int -> List Int -> Html Msg
viewTypingText targetText currentPosition correctedPositions =
    let
        chars =
            String.toList targetText
                |> List.indexedMap (\index char -> ( index, String.fromChar char ))

        renderChar ( index, char ) =
            let
                charClass =
                    if index < currentPosition then
                        if List.member index correctedPositions then
                            "char-corrected"
                        else
                            "char-correct"
                    else if index == currentPosition then
                        "char-current"
                    else
                        "char-remaining"
            in
            span [ class charClass ] [ text char ]
    in
    div [ class "text-display" ]
        (List.map renderChar chars)