module Osu exposing (..)

------Imports----------
import Browser
import Html exposing (..)
import Debug

-----------------------

main : Program Flags Model Msg
main = 
   Browser.element
     { init = init
     , view = view 
     , update = update 
     , subscription = subscription
     }

type Msg = Debug.todo "todo"

type alias Flags = ()

init : Flags -> (Model,Cmd Msg)
init () = 
    Debug.todo "TODO"

initModel : Model
initModel =
    Debug.todo "TODO"


update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  Debug.todo "TODO"

subscriptions : Model -> Sub Msg
subscriptions model =
  Debug.todo "TODO"

view: Model -> Html Msg
view model = 
  debug.todo "TODO"
         
