port module Main exposing (..)

import AnimationFrame
import Html


{- src -}

import Models exposing (Model, initialModel, NetworkState(..))
import View exposing (view)
import Update exposing (update, Msg(..))
import Window
import Task


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == Going then
            AnimationFrame.times Learn
          else
            Sub.none
        , Window.resizes decodeWindowSize
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform decodeWindowSize Window.size )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )

-}
decodeWindowSize : Window.Size -> Msg
decodeWindowSize size =
    WindowResize ( size.width, size.height )


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    ( Maybe.withDefault initialModel savedModel, Task.perform decodeWindowSize Window.size )
