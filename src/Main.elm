module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Random


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
    | UpdateInput String
    | StartOver
    | GotRandomIndex Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { meditations = []
      , currentMeditation = Nothing
      , userInput = ""
      , currentPosition = 0
      , isComplete = False
      , mistakes = 0
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
            ( { model | meditations = filteredMeditations }
            , Random.generate GotRandomIndex (Random.int 0 (List.length filteredMeditations - 1))
            )

        LoadMeditations (Err _) ->
            ( model, Cmd.none )

        GotRandomIndex index ->
            let
                selectedMeditation =
                    List.drop index model.meditations |> List.head
            in
            ( { model | currentMeditation = selectedMeditation }, Cmd.none )

        SelectRandomMeditation _ ->
            ( model
            , Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1))
            )

        UpdateInput input ->
            case model.currentMeditation of
                Nothing ->
                    ( model, Cmd.none )

                Just meditation ->
                    let
                        targetText =
                            String.slice 0 (String.length input) meditation.text

                        newMistakes =
                            if input == targetText then
                                model.mistakes

                            else
                                model.mistakes + 1

                        newPosition =
                            String.length input

                        isComplete =
                            input == meditation.text
                    in
                    ( { model
                        | userInput = input
                        , currentPosition = newPosition
                        , mistakes = newMistakes
                        , isComplete = isComplete
                      }
                    , Cmd.none
                    )

        StartOver ->
            ( { model
                | userInput = ""
                , currentPosition = 0
                , isComplete = False
                , mistakes = 0
              }
            , Random.generate GotRandomIndex (Random.int 0 (List.length model.meditations - 1))
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title" ] [ text "Daily Typing Practice" ]
        , h2 [ class "subtitle" ] [ text "명상록으로 하루를 시작하세요" ]
        , case model.currentMeditation of
            Nothing ->
                div [ class "loading" ] [ text "명상록을 불러오는 중..." ]

            Just meditation ->
                div [ class "practice-area" ]
                    [ div [ class "meditation-info" ]
                        [ h3 [] [ text meditation.author ]
                        , p [ class "source" ] [ text meditation.source ]
                        ]
                    , div [ class "typing-area" ]
                        [ div [ class "target-text" ]
                            [ viewTargetText meditation.text model.userInput model.currentPosition ]
                        , textarea
                            [ class "input-area"
                            , placeholder "여기에 타이핑하세요..."
                            , value model.userInput
                            , onInput UpdateInput
                            , disabled model.isComplete
                            ]
                            []
                        ]
                    , div [ class "stats" ]
                        [ div [ class "progress" ]
                            [ text ("진행률: " ++ String.fromInt (round ((toFloat model.currentPosition / toFloat (String.length meditation.text)) * 100)) ++ "%") ]
                        , div [ class "mistakes" ]
                            [ text ("실수: " ++ String.fromInt model.mistakes ++ "회") ]
                        ]
                    , if model.isComplete then
                        div [ class "completion" ]
                            [ h3 [] [ text "완료!" ]
                            , p [] [ text "명상록 한 구절을 완성했습니다." ]
                            , button [ onClick StartOver, class "btn-primary" ] [ text "새로운 구절로 시작" ]
                            ]

                      else
                        div [ class "controls" ]
                            [ button [ onClick StartOver, class "btn-secondary" ] [ text "다시 시작" ] ]
                    ]
        ]


viewTargetText : String -> String -> Int -> Html Msg
viewTargetText targetText userInput currentPosition =
    let
        typed =
            String.slice 0 currentPosition targetText

        current =
            String.slice currentPosition (currentPosition + 1) targetText

        remaining =
            String.slice (currentPosition + 1) (String.length targetText) targetText

        typedCorrect =
            String.slice 0 (String.length userInput) targetText == userInput

        typedClass =
            if typedCorrect then
                "typed-correct"

            else
                "typed-incorrect"
    in
    div [ class "text-display" ]
        [ span [ class typedClass ] [ text typed ]
        , span [ class "current-char" ] [ text current ]
        , span [ class "remaining" ] [ text remaining ]
        ]