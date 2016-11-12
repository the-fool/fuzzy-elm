module SvgViews exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Models exposing (..)


largeChart : Float -> List ( Float, Float, Int ) -> Html.Html a
largeChart dim data =
    let
        factor =
            (+) 5 >> (*) (dim / 10)

        normalizedData =
            List.map (\( x, y, c ) -> ( factor x, factor y, c )) data

        s =
            toString

        fillColor label =
            if label == 1 then
                colors.positive
            else
                colors.negative

        toCircle ( x, y, c ) =
            circle [ cx (s x), cy (s y), r (s 3), stroke "white", strokeWidth "1", fillColor c |> fill ] []
    in
        svg
            [ version "1.1", x "0", y "0", width <| toString dim, height <| toString dim ]
            (List.map toCircle normalizedData)
