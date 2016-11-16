module CanvasViz exposing (..)

import Datasets


toCanvasData : Datasets.AggregatedPredictions -> List (List (List Int))
toCanvasData =
    let
        opacity =
            120

        toRgba point =
            if point < 0 then
                [ 245, 147, 34 ] ++ [ opacity ]
            else
                [ 8, 119, 189 ] ++ [ opacity ]
    in
        List.map (List.map (List.concatMap toRgba))
