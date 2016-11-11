module SvgViews exposing (..)

import Random.Pcg as Random
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Models exposing (..)


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> snd


randomPairs : Int -> List ( Float, Float )
randomPairs len =
    let
        gen =
            Random.pair (Random.float -1 1) (Random.float -1 1)
                |> Random.list len
    in
        Random.step gen seed0 |> fst


largeChart : Float -> List ( Float, Float, Int ) -> Html.Html a
largeChart dim data =
    let
        factor =
            (+) 1 >> (*) (dim / 2)

        normalizedData =
            List.map (\( x, y, c ) -> ( factor x, factor y, c )) data

        s =
            toString

        toCircle ( x, y, c ) =
            circle [ cx (s x), cy (s y), r (s 3), stroke "white", strokeWidth "1", fill colors.positive ] []
    in
        svg
            [ version "1.1", x "0", y "0", width <| toString dim, height <| toString dim ]
            (List.map toCircle normalizedData)


xorData : List (Int Int Int)
xorData =
    []
