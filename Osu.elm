port module Osu exposing (..)

import Audio exposing (Audio, AudioCmd, AudioData)
import Duration
import Html exposing (..)
import Html.Events
import Json.Decode
import Json.Encode
import List.Nonempty exposing (Nonempty(..))
import Task
import Time
import Color exposing (..)
--import Canvas exposing (Shape)
--import Canvas.Settings exposing (..)
--import Html.Attributes exposing (attribute)
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    , hitCount : Int
    }


type SoundState
    = NotPlaying
    | Playing Time.Posix
    | FadingOut Time.Posix Time.Posix

-- MODEL

type Model
    = LoadingModel
    | LoadedModel LoadedModel_
    | LoadFailedModel

type alias Point = { x:Float, y:Float }

type Msg
    = SoundLoaded (Result Audio.LoadError Audio.Source)
    | PressedPlay
    | PressedPlayAndGotTime Time.Posix
    | PressedStop
    | PressedStopAndGotTime Time.Posix
    | RandomTarget Point 
    | Hit Point


init : flags -> ( Model, Cmd Msg, AudioCmd Msg )
init _ =
    ( LoadingModel
    , Cmd.none
    , Audio.loadAudio
        SoundLoaded
        "./party-in-me.mp3"
    )

-- UPDATE

update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg model =
    case ( msg, model ) of
        ( SoundLoaded result, LoadingModel ) ->
            case result of
                Ok sound ->
                    ( LoadedModel { sound = sound, soundState = NotPlaying, hitCount = 0 }
                    , Cmd.none
                    , Audio.cmdNone
                    )
                Err _ ->
                    ( LoadFailedModel
                    , Cmd.none
                    , Audio.cmdNone
                    )
        ( PressedPlay, LoadedModel loadedModel ) ->
            ( LoadedModel loadedModel
            , Task.perform PressedPlayAndGotTime Time.now
            , Audio.cmdNone
            )
        ( PressedPlayAndGotTime time, LoadedModel loadedModel ) ->
            ( LoadedModel { loadedModel | soundState = Playing time }
            , Cmd.none
            , Audio.cmdNone
            )
        ( PressedStop, LoadedModel loadedModel ) ->
            ( LoadedModel loadedModel
            , Task.perform PressedStopAndGotTime Time.now
            , Audio.cmdNone
            )
        ( PressedStopAndGotTime stopTime, LoadedModel loadedModel ) ->
            case loadedModel.soundState of
                Playing startTime ->
                    ( LoadedModel { loadedModel | soundState = FadingOut startTime stopTime }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                _ ->
                    ( model, Cmd.none, Audio.cmdNone )
        _ ->
            ( model, Cmd.none, Audio.cmdNone )

-- VIEW

view : AudioData -> Model -> Html Msg
view _ model =
    case model of
        LoadingModel ->
            Html.text "Loading..."
        LoadedModel loadingModel ->
            case loadingModel.soundState of
                Playing _ ->
                    Html.div
                        []
                        [ Html.button [ Html.Events.onClick PressedStop ] [ Html.text "Stop music" ]
                        , svg
                          [ width "1100"
                          , height "700"
                          , viewBox "0 50 1100 700"
                          ]
                          [ circle
                            [ cx "550"
                            , cy "350"
                            , r "50"
                            ]
                            []
                          ]
                        ]
                _ ->
                    Html.div
                        []
                        [ Html.button [ Html.Events.onClick PressedPlay ] [ Html.text "Play music!" ] ]
        LoadFailedModel ->
            Html.text "Failed to load sound."

-- AUDIO

audio : AudioData -> Model -> Audio
audio _ model =
    case model of
        LoadedModel loadedModel ->
            case loadedModel.soundState of
                NotPlaying ->
                    Audio.silence

                Playing time ->
                    Audio.audio loadedModel.sound time

                FadingOut startTime stopTime ->
                    Audio.audio loadedModel.sound startTime
                        |> Audio.scaleVolumeAt [ ( stopTime, 1 ), ( Duration.addTo stopTime (Duration.seconds 2), 0 ) ]
        _ ->
            Audio.silence


port audioPortToJS : Json.Encode.Value -> Cmd msg

port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

-- MAIN
main : Platform.Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ _ -> Sub.none
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }

-- DRAWING CIRCLES