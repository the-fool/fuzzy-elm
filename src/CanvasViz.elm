module CanvasViz exposing (..)

import Network exposing (Network)
import List.Extra
import String


type alias RgbaGrid =
    List (List (List Int))


type alias NeuronDatum =
    { id : String, data : List Int }


type alias CanvasMessage =
    { output : List Int, neurons : List NeuronDatum }


makeCanvasMessage : Network.AggregatedPredictions -> String -> CanvasMessage
makeCanvasMessage predictions jumboID =
    let
        neurons =
            predictions |> toRgba |> toCanvasData

        output =
            case List.Extra.find (.id >> (==) jumboID) neurons of
                Just item ->
                    item.data

                Nothing ->
                    Debug.crash ("Unfound ID for jumbo neuron!  Id: " ++ jumboID ++ "Neurons: " ++ (String.join " " (List.map (.id) neurons)))
    in
        { output = output, neurons = neurons }


canvasMessage : Network -> String -> CanvasMessage
canvasMessage a b =
    { output = [], neurons = [] }


toCanvasData : RgbaGrid -> List NeuronDatum
toCanvasData grid =
    let
        idDataRecorder id data =
            { id = id, data = data }
    in
        Network.gridPrism idDataRecorder grid |> List.concat


toRgba : Network.AggregatedPredictions -> RgbaGrid
toRgba =
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
