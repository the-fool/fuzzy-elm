module Canvas exposing (..)

import Native.Canvas
import Buffer exposing (Buffer)
import Network exposing (Network)
import Task


drawCanvases : Buffer -> Buffer
drawCanvases =
    Native.Canvas.drawCanvases


paintEntry : Network -> Cmd a
paintEntry network =
    let
        buffer =
            Buffer.buffer (List.length network.entryNeurons)

        payload =
            network.entryNeurons
                |> List.map .neuron
                |> List.indexedMap (\i n -> Buffer.set i n buffer)
                |> always buffer
    in
        drawCanvases payload |> always Cmd.none


generateCanvasPayload : Network -> Task.Task x Buffer
generateCanvasPayload network =
    let
        hidden =
            network.layers
                |> List.concat
    in
        (network.outputNeuron :: hidden)
            |> List.indexedMap (\i el -> Buffer.set i el network.canvasPayload)
            |> always network.canvasPayload
            |> Task.succeed


paintCanvas : Buffer -> Cmd a
paintCanvas payload =
    drawCanvases payload |> always Cmd.none
