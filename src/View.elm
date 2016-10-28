module View exposing (..)

import Html exposing (..)
import Models exposing (..)
import Messages exposing (Msg(..))


view : AppModel -> Html Msg
view model =
    div []
        [ text "Hey, Fuzzy Tools!" ]
