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
import Svg.Events exposing(..)


type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    , hitCount : Int
    , points : List Point
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
    | RandomPoint Point 
    | Hit Point
    | Tick


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
        (RandomPoint point,LoadedModel loadedModel) ->
           ( LoadedModel {sound = loadedModel.sound, soundState = loadedModel.soundState, 
            hitCount = loadedModel.hitCount, points = loadedModel.points ++[point]}
             ,Cmd.none
             ,Audio.cmdNone)      

        ( SoundLoaded result, LoadingModel ) ->
            case result of
                Ok sound ->
                    ( LoadedModel { sound = sound, soundState = NotPlaying, hitCount = 0, points = []}
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
        
        (Tick,_) -> (model, Random.generate RandomPoint pointGenerator, Audio.cmdNone)
             
        (Hit point ,LoadedModel lM) ->
             (LoadedModel {lM | hitCount = lM.hitCount +1, 
              points = (remove point lM.points)}
             ,Cmd.none,Audio.cmdNone)
        _ ->
            ( model, Cmd.none, Audio.cmdNone )



--take a hit point and remove it from the list
remove : Point -> List Point -> List Point
remove p list =
  case list of
    [] -> []
    
    x::xs ->
        if (x == p) then
           xs
        else
            [x] ++ (remove p xs)  
        
--SUBSCRIPTION
subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ model =
    Time.every 500 (always Tick)



pointGenerator : Generator Point
pointGenerator =
  let
    x = ( Random.float 50 1000) 
    y = ( Random.float 50 600)
  in
    Random.map2 Point x y 


pointToCircle: String -> Point -> Svg Msg
pointToCircle foo bar = 
    circle [ cx (String.fromFloat bar.x)
             ,cy (String.fromFloat bar.y)
             ,r "5"
             ,fill foo
             ,onClick(Hit (bar))  
           ]
           [] 
        
pointToCircles: String -> List Point -> List (Svg Msg)  
pointToCircles colors points = 
    List.map (pointToCircle colors) points




-- VIEW

view : AudioData -> Model -> Html Msg
view _ model =
    case model of
        LoadingModel ->
            Html.text "Loading..."
        LoadedModel loadingModel ->
            case loadingModel.soundState of
                Playing _ ->
                    let 
                       list = loadingModel.points
                       dots = pointToCircles "red" list
                     in
                      Html.div
                        []
                        [ Html.button [ Html.Events.onClick PressedStop ] [ Html.text "Stop music" ]
                        , div [] 
                          [Html.text ("Score: " ++ 
                          (String.fromInt loadingModel.hitCount))]
                        ,svg
                          [ width "1100"
                          , height "700"
                          , viewBox "50 50 1100 700"
                          ]
                                     
                          dots
                                                
                                                  
                            
                          
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
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }

-- DRAWING CIRCLES
