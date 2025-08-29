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
    , sessionState : SessionState
    , sessionStartTime : Maybe Time.Posix
    , selectedSessionDuration : Int  -- in minutes
    , completedSessions : Int
    , gracePeriodStartTime : Maybe Time.Posix
    , textRetryCount : Int  -- Number of retries for current text
    }

type SessionState
    = SessionNotStarted
    | SessionActive
    | SessionGracePeriod  -- New state for grace period
    | SessionCompleted

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
    | GotRandomIndex Int
    | Tick Time.Posix
    | FocusTypingArea
    | StartSession
    | EndSession
    | SelectSessionDuration Int


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
      , sessionState = SessionNotStarted
      , sessionStartTime = Nothing
      , selectedSessionDuration = 10  -- default 10 minutes
      , completedSessions = 0
      , gracePeriodStartTime = Nothing
      , textRetryCount = 0
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
                    if model.isComplete then
                        -- Ignore all keys when completed (auto-advance already handled)
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

                            -- Set session start time on first correct keystroke
                            newSessionStartTime =
                                case model.sessionStartTime of
                                    Nothing ->
                                        if result.position > model.currentPosition && model.sessionState == SessionActive then
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
                            -- Retry system: reset with full hearts or advance to next text after 3 retries
                            let
                                newRetryCount = model.textRetryCount + 1
                                
                                newSessionState = 
                                    if model.sessionState == SessionGracePeriod then
                                        SessionCompleted
                                    else
                                        model.sessionState
                                
                                shouldLoadNextText = 
                                    (model.sessionState == SessionActive) && (newRetryCount > 3)
                                
                                updatedModel = 
                                    { model
                                        | userInput = ""
                                        , currentPosition = 0
                                        , mistakes = 0
                                        , correctedPositions = []
                                        , startTime = Nothing
                                        , endTime = Nothing
                                        , isComplete = False
                                        , sessionState = newSessionState
                                        , textRetryCount = if shouldLoadNextText then 0 else newRetryCount
                                    }
                            in
                            if shouldLoadNextText then
                                ( updatedModel, Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1)) )
                            else
                                ( updatedModel, Cmd.none )
                        else if isComplete then
                            -- Auto advance to next text when completed
                            let
                                newCompletedSessions =
                                    if model.sessionState == SessionActive || model.sessionState == SessionGracePeriod then
                                        model.completedSessions + 1
                                    else
                                        model.completedSessions
                                
                                -- End session when text is completed during grace period
                                newSessionState =
                                    if model.sessionState == SessionGracePeriod then
                                        SessionCompleted
                                    else
                                        model.sessionState
                            in
                            let
                                shouldLoadNextText = model.sessionState == SessionActive
                                
                                updatedModel = 
                                    { model
                                        | userInput = ""
                                        , currentPosition = 0
                                        , isComplete = False
                                        , mistakes = 0
                                        , correctedPositions = []
                                        , startTime = Nothing
                                        , endTime = Nothing
                                        , completedSessions = newCompletedSessions
                                        , sessionState = newSessionState
                                        , textRetryCount = 0  -- Reset retry count on success
                                    }
                            in
                            if shouldLoadNextText then
                                ( updatedModel, Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1)) )
                            else
                                ( updatedModel, Cmd.none )
                        else
                            ( { model
                                | userInput = result.userInput
                                , currentPosition = result.position
                                , mistakes = result.mistakes
                                , isComplete = isComplete
                                , correctedPositions = result.correctedPositions
                                , startTime = newStartTime
                                , endTime = newEndTime
                                , sessionStartTime = newSessionStartTime
                              }
                            , Cmd.none
                            )


        Tick time ->
            let
                updatedModel = { model | currentTime = time }
                
                -- Check if session should end and start grace period
                sessionShouldEnd = 
                    case (model.sessionState, model.sessionStartTime) of
                        (SessionActive, Just startTime) ->
                            let
                                durationMillis = model.selectedSessionDuration * 60 * 1000
                            in
                            Time.posixToMillis time - Time.posixToMillis startTime >= durationMillis
                        _ ->
                            False
                
            in
            if sessionShouldEnd && not model.isComplete then
                -- Start grace period if text is not complete (silent mode)
                ( { updatedModel 
                    | sessionState = SessionGracePeriod
                    , gracePeriodStartTime = Just time
                  }, Task.attempt (\_ -> FocusTypingArea) (Dom.focus "typing-area") )
            else if sessionShouldEnd && model.isComplete then
                -- End session immediately if text is complete
                ( { updatedModel | sessionState = SessionCompleted }, Cmd.none )
            else
                ( updatedModel, Cmd.none )

        StartSession ->
            ( { model 
                | sessionState = SessionActive
                , sessionStartTime = Nothing  -- Will be set on first correct keystroke
                , completedSessions = 0
                , textRetryCount = 0  -- Reset retry count for new session
              }
            , Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1))
            )

        EndSession ->
            ( { model 
                | sessionState = SessionNotStarted
                , sessionStartTime = Nothing
                , completedSessions = 0
                , gracePeriodStartTime = Nothing
                , textRetryCount = 0
              }
            , Cmd.none
            )

        SelectSessionDuration minutes ->
            ( { model | selectedSessionDuration = minutes }
            , Cmd.none
            )



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
    3 + (wordCount // 30)


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


calculateSessionTimeLeft : Model -> Int
calculateSessionTimeLeft model =
    case (model.sessionState, model.sessionStartTime) of
        (SessionActive, Just startTime) ->
            let
                durationMillis = model.selectedSessionDuration * 60 * 1000
                elapsed = Time.posixToMillis model.currentTime - Time.posixToMillis startTime
                remaining = Basics.max 0 (durationMillis - elapsed)
            in
            remaining // 1000  -- Convert to seconds
        (SessionActive, Nothing) ->
            model.selectedSessionDuration * 60  -- Full time when not started
        _ ->
            model.selectedSessionDuration * 60  -- selected duration in seconds


formatSessionTime : Int -> String
formatSessionTime totalSeconds =
    let
        minutes = totalSeconds // 60
        seconds = modBy 60 totalSeconds
    in
    String.padLeft 2 '0' (String.fromInt minutes) ++ ":" ++ String.padLeft 2 '0' (String.fromInt seconds)



view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title" ] [ text "Daily Typing Practice" ]
        , h2 [ class "subtitle" ] [ text "명상록으로 하루를 시작하세요" ]
        , case model.sessionState of
            SessionNotStarted ->
                viewSessionTimer model
            
            SessionActive ->
                div []
                    [ viewSessionTimer model
                    , case model.currentMeditation of
                        Nothing ->
                            div [ class "loading" ] [ text "명상록을 불러오는 중..." ]
                        
                        Just meditation ->
                            viewTypingPractice model meditation
                    ]
            
            SessionGracePeriod ->
                div []
                    [ viewSessionTimer model
                    , div [ class "grace-period-message" ]
                        [ text "⏰ 세션 시간이 종료되었습니다. 남은 기회동안 계속 입력해주세요!" ]
                    , case model.currentMeditation of
                        Nothing ->
                            div [ class "loading" ] [ text "명상록을 불러오는 중..." ]
                        
                        Just meditation ->
                            viewTypingPractice model meditation
                    ]
            
            SessionCompleted ->
                viewSessionTimer model
        ]



viewTypingPractice : Model -> Meditation -> Html Msg
viewTypingPractice model meditation =
    div [ class "practice-area" ]
        [ div [ class "typing-area" ]
            [ viewSessionProgressBar model
            , div 
                [ class "target-text"
                , tabindex 0
                , id "typing-area"
                , on "keydown" (Json.map KeyPressed (Json.field "key" Json.string))
                ]
                [ viewTypingText meditation.text model.currentPosition model.correctedPositions ]
            , div [ class "text-lives" ]
                [ div [ class "lives-label" ] [ text "이 지문 도전 기회:" ]
                , div [ class "lives-display" ] (viewLives model meditation.text)
                , if model.mistakes >= (calculateMaxLives model meditation.text) then
                    div [ class "lives-warning" ]
                        [ text "⚠️ 기회를 모두 사용했습니다! 다음 오타 시 처음부터 다시 시작됩니다." ]
                  else
                    text ""
                ]
            , div [ class "meditation-info-small" ]
                [ span [ class "author-small" ] [ text meditation.author ]
                , span [ class "source-small" ] [ text (" · " ++ meditation.source) ]
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
                ]
          else
            text ""  -- No restart button anymore
        ]


viewSessionTimer : Model -> Html Msg
viewSessionTimer model =
    div [ class "session-timer" ]
        [ case model.sessionState of
            SessionNotStarted ->
                div [ class "session-start" ]
                    [ h3 [] [ text "⏱️ 세션 타이머" ]
                    , p [] [ text "집중 세션을 시작해보세요!" ]
                    , div [ class "duration-selector" ]
                        [ h4 [] [ text "세션 시간 선택:" ]
                        , div [ class "duration-buttons" ]
                            [ button 
                                [ onClick (SelectSessionDuration 1)
                                , class (if model.selectedSessionDuration == 1 then "btn-duration active" else "btn-duration")
                                ] 
                                [ text "1분" ]
                            , button 
                                [ onClick (SelectSessionDuration 5)
                                , class (if model.selectedSessionDuration == 5 then "btn-duration active" else "btn-duration")
                                ] 
                                [ text "5분" ]
                            , button 
                                [ onClick (SelectSessionDuration 10)
                                , class (if model.selectedSessionDuration == 10 then "btn-duration active" else "btn-duration")
                                ] 
                                [ text "10분" ]
                            , button 
                                [ onClick (SelectSessionDuration 15)
                                , class (if model.selectedSessionDuration == 15 then "btn-duration active" else "btn-duration")
                                ] 
                                [ text "15분" ]
                            ]
                        ]
                    , button [ onClick StartSession, class "btn-session-start" ] 
                        [ text (String.fromInt model.selectedSessionDuration ++ "분 세션 시작") ]
                    ]
            
            SessionActive ->
                div [ class "session-active-compact" ]
                    [ div [ class "session-info-compact" ]
                        [ div [ class "session-time-compact" ]
                            [ text (
                                case model.sessionStartTime of
                                    Nothing -> "⏱️ " ++ formatSessionTime (calculateSessionTimeLeft model) ++ " (타이핑 시작 대기중)"
                                    Just _ -> "⏱️ " ++ formatSessionTime (calculateSessionTimeLeft model)
                            ) ]
                        , div [ class "session-completed-compact" ]
                            [ text ("✅ " ++ String.fromInt model.completedSessions ++ "개 완성") ]
                        , button [ onClick EndSession, class "btn-session-stop-compact" ] [ text "종료" ]
                        ]
                    ]
            
            SessionGracePeriod ->
                -- Don't show timer during grace period
                text ""
            
            SessionCompleted ->
                div [ class "session-completed" ]
                    [ h3 [] [ text "🎉 세션 완료!" ]
                    , div [ class "session-results" ]
                        [ p [] [ text (String.fromInt model.selectedSessionDuration ++ "분 동안 " ++ String.fromInt model.completedSessions ++ "개의 지문을 완성했습니다!") ]
                        , div [ class "session-actions" ]
                            [ button [ onClick StartSession, class "btn-session-start" ] [ text "새 세션 시작" ]
                            , button [ onClick EndSession, class "btn-session-stop" ] [ text "종료" ]
                            ]
                        ]
                    ]
        ]


viewSessionProgressBar : Model -> Html Msg
viewSessionProgressBar model =
    case model.sessionState of
        SessionActive ->
            let
                timeLeft = calculateSessionTimeLeft model
                totalTime = model.selectedSessionDuration * 60
                -- Keep 100% when session hasn't started yet
                remainingPercentage = 
                    case model.sessionStartTime of
                        Nothing -> 100.0
                        Just _ -> (toFloat timeLeft / toFloat totalTime) * 100
                
                progressStyle =
                    style "width" (String.fromFloat remainingPercentage ++ "%")
            in
            div [ class "session-progress-container" ]
                [ div [ class "session-progress-bar" ]
                    [ div [ class "session-progress-fill", progressStyle ] []
                    ]
                ]
        _ ->
            text ""


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