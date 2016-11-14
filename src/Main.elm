port module Main exposing (..)

import AnimationFrame
import Html.App as App


{- src -}

import Models exposing (Model, emptyModel)
import View exposing (view)
import Update exposing (update, Msg(..))
import Window
import Task


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.state == 1 then
            AnimationFrame.diffs Begin
          else
            Sub.none
        , Window.resizes decodeWindowSize
        ]


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Window.resizes decodeWindowSize
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )


decodeWindowSize : Window.Size -> Msg
decodeWindowSize size =
    WindowResize ( size.width, size.height )


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    ( Maybe.withDefault emptyModel savedModel, Task.perform (always NoOp) decodeWindowSize Window.size )
