port module Osu exposing (..)

import Audio exposing (Audio, AudioCmd, AudioData)
import Duration
import Html exposing (..)
import Html.Events
import Html.Attributes
import Json.Decode
import Json.Encode
import List.Nonempty exposing (Nonempty(..))
import Task
import Time
import Style
import Style.Color
import Style.Font as Font
--import Color exposing (..)
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing(..)
import Color exposing (white)
import Html.Attributes
import Ellipse2d exposing (yRadius)

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
        
        (Tick,LoadedModel lm) ->
          case lm.soundState of
             Playing time ->
                 (model, Random.generate RandomPoint pointGenerator, 
                 Audio.cmdNone)
             _ -> (model,Cmd.none,Audio.cmdNone)
             
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
    Time.every 1000 (always Tick)


--creates a random point
pointGenerator : Generator Point
pointGenerator =
  let
    x = ( Random.float 100 1080) 
    y = ( Random.float 100 580)
  in
    Random.map2 Point x y 


pointToCircle: String -> Point -> Svg Msg
pointToCircle foo bar = 
    circle [ cx (String.fromFloat bar.x)
             ,cy (String.fromFloat bar.y)
             ,r "10"
             ,fill foo
             ,stroke "purple"
             ,onClick(Hit (bar))  
           ]
           [] 
        
pointToCircles: String -> List Point -> List (Svg Msg)  
pointToCircles colors points = 
    List.map (pointToCircle colors) points


buildBoard : Svg Msg
buildBoard = 
    rect [ x "90"
        , y "90"
        , width  "1000"
        , height "500"
        ,fill "white"
        ,stroke "purple"
        ,strokeWidth "3"
        ,rx "10px"
        ,ry "10px"] []
                                    

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
                       dots = pointToCircles "#fae5fc" list
                       board = buildBoard 
                                    
                                
                       totalRender = [board] ++ dots
                      in
                      Html.div
                        []
                        [ Html.button ([ Html.Events.onClick PressedStop] ++ endButtonStyle) [ Html.text "End Game" ]
                        , div [Html.Attributes.style "font" "30px Verdana, sans-serif"
                          , Html.Attributes.style "padding" "35 0 10 465"] 
                          [Html.text ("Score: " ++ 
                          (String.fromInt loadingModel.hitCount))]
                        , svg
                          [ width "1100"
                          , height "700"
                          , viewBox "50 50 1100 700"
                          ]
                          totalRender       
                        ]
                _ ->
                    Html.div
                        [Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "align-items" "center"
                        , Html.Attributes.style "justify-content" "center"]
                        [ Html.button ([ Html.Events.onClick PressedPlay ] ++ startButtonStyle) [ Html.text "Play!" ] ]
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

-- STYLES

endButtonStyle : List (Html.Attribute msg)
endButtonStyle =
    [ Html.Attributes.style "width" "300px"
    , Html.Attributes.style "background-color" "#fae5fc"
    , Html.Attributes.style "color" "purple"
    , Html.Attributes.style "margin-top" "10px"
    , Html.Attributes.style "margin-left" "380px"
    , Html.Attributes.style "border-color" "purple"
    , Html.Attributes.style "border-width" "2px"
    , Html.Attributes.style "border-radius" "8px"
    , Html.Attributes.style "font" "20px Verdana, sans-serif"
    , Html.Attributes.style "padding" "10 10 10 10"
    ]

startButtonStyle : List (Html.Attribute msg)
startButtonStyle =
    [ Html.Attributes.style "width" "500px"
    , Html.Attributes.style "background-color" "#e4fccf"
    , Html.Attributes.style "color" "green"
    , Html.Attributes.style "margin-top" "200px"
    , Html.Attributes.style "border-color" "green"
    , Html.Attributes.style "border-width" "3px"
    , Html.Attributes.style "border-radius" "12px"
    , Html.Attributes.style "font" "50px Verdana, sans-serif"
    , Html.Attributes.style "padding" "50 80 50 80"
    ]
