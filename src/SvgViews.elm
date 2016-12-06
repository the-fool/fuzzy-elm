module SvgViews exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (on)
import Core
import Datasets
import Array exposing (Array)
import Update exposing (paintBrusher)


largeChart : Int -> Array Datasets.Point -> Html.Html Update.Msg
largeChart dim data =
    let
        factor =
            (+) Core.dataRange >> (*) (toFloat dim / (Core.dataRange * 2))

        normalizedData =
            Array.map (\datum -> { datum | coord = ( Tuple.first datum.coord |> factor, Tuple.second datum.coord |> factor ) }) data
                |> Array.toList

        s =
            toString

        fillColor label =
            if label == 1 then
                Core.colors |> .positive
            else
                Core.colors |> .negative

        toCircle { coord, label } =
            circle [ cx (Tuple.first coord |> s), cy (Tuple.second coord |> s), r (s 3), stroke "white", strokeWidth "1", fillColor label |> fill ] []
    in
        svg
            [ version "1.1"
            , x "0"
            , y "0"
            , width <| toString dim
            , height <| toString dim
            , Html.Attributes.style [ ( "position", "relative" ) ]
            , Svg.Events.on "mousedown" (paintBrusher dim)
            ]
            (List.map toCircle normalizedData)
