module Constants exposing (..)

import List.Extra exposing (lift2)

colors : { positive : String, negative : String }
colors =
    { negative = "rgb(245, 147, 34)"
    , positive = "rgb(8, 119, 189)"
    }


dataRange : Float
dataRange =
    5


density : Int
density =
    100


jumboCanvasSize : Int
jumboCanvasSize =
    100

brutePoints : List ( Float, Float )
brutePoints =
    let
        scale (min, max) (a,b) x =
            (b - a) * (x - min) / (max - min) + a

        scaleFun =
            scale ( 0, toFloat density ) ( -dataRange, dataRange )

        scaledInputs =
            List.map (toFloat >> scaleFun) [0..(density - 1)]
    in
        lift2 (\y x -> ( x, y )) scaledInputs scaledInputs
