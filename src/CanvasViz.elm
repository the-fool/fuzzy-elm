module CanvasViz exposing (..)

import Network
import List.Extra exposing (lift2)
import Datasets exposing (dataRange)


density : Float
density =
    99


scale : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
scale domain range x =
    let
        ( min, max ) =
            domain

        ( a, b ) =
            range
    in
        (b - a) * (x - min) / (max - min) + a



--    Produce list of network outputs (in the form of matrices) for each input point.
--    There will be density ^ 2 points


brutePredictions : Network.Network -> List (List (List Float))
brutePredictions network =
    let
        scaleFun =
            scale ( 0, density ) ( -dataRange, dataRange )

        scaledInputs =
            List.map scaleFun [0..density]

        points =
            lift2 (\y x -> ( x, y )) scaledInputs scaledInputs
    in
        -- Drop 1 to ignore the original input element
        List.map ((Network.forwardProp network) >> (List.drop 1)) points
