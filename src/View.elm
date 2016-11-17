module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import String
import Models exposing (..)
import Network exposing (..)
import Update exposing (Msg(..))
import SvgViews
import Datasets exposing (xorData)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type alias Geometry =
    { vertical : Int
    , boxSize : Int
    , datasetsPcnt : Float
    , networkPcnt : Float
    , outputBox : Float
    }


geometry : Geometry
geometry =
    { vertical = 75
    , boxSize = 45
    , datasetsPcnt = 0.1
    , networkPcnt = 0.6
    , outputBox = 300
    }


wrapperWidth : Int -> Int
wrapperWidth x =
    if x >= 1400 then
        1300
    else if x >= 1100 then
        1000
    else
        900


view : Model -> Html Msg
view model =
    let
        nonOutputLayers =
            model.network.layers
                |> List.take (List.length model.network.layers - 1)

        maxWidth =
            fst model.window |> wrapperWidth

        factor x =
            maxWidth |> toFloat |> (*) x |> truncate

        column sz node =
            div [ class "col", style [ "width" => (px <| factor sz) ] ] [ node ]

        ( datasetsWidth, networkWidth, outputWidth ) =
            ( geometry.datasetsPcnt, geometry.networkPcnt, (1 - (geometry.datasetsPcnt + geometry.networkPcnt)) )
    in
        div
            [ class "main-wrapper" ]
            [ header
            , div [ class "ui-wrapper clearfix mx-auto", style [ "width" => px maxWidth ] ]
                [ controls model
                , div [ class "visuals" ]
                    [ column datasetsWidth <| dataSets model
                    , column networkWidth <| network (factor networkWidth) nonOutputLayers
                    , column outputWidth <| output model
                    ]
                ]
            ]


header : Html Msg
header =
    div
        [ class "clearfix mb2 white bg-black"
        ]
        [ div
            [ class "left p2" ]
            [ Html.text "Elm Brain" ]
        ]


controls : Model -> Html Msg
controls model =
    let
        toggleButton =
            if model.state == 0 then
                button [ class "btn", onClick Begin ] [ text "Go" ]
            else
                button [ class "btn", onClick Pause ] [ text "Stop" ]

        reset =
            button [ class "btn", onClick Reset ] [ text "Reset" ]

        ticker =
            span [] [ toString model.nTicks |> text ]
    in
        div [ class "controls" ]
            [ h1 [] [ text "controls" ]
            , reset
            , toggleButton
            , ticker
            ]


dataSets : Model -> Html Msg
dataSets model =
    let
        dataSelector ( name, handler ) =
            div
                [ handler model.randomSeed
                    |> SetInput
                    |> onClick
                ]
                [ text name ]

        dataOptions =
            [ ( "XOR", xorData ), ( "GAUSSIAN", xorData ) ]
    in
        div
            [ class "datasets-wrapper" ]
            (List.map dataSelector dataOptions)


network : Int -> List Layer -> Html Msg
network networkWidth layers =
    let
        entry =
            case List.head layers of
                Just entryLayer ->
                    entryLayer

                Nothing ->
                    Debug.crash "Nothing in entry layer"

        hidden =
            List.drop 1 layers

        gutter =
            (*) <| (networkWidth - geometry.boxSize - 20) // (List.length hidden)
    in
        div
            [ class "network-wrapper" ]
            [ div
                [ class "layer-editor-wrapper"
                , style [ "margin-left" => px geometry.boxSize ]
                ]
                [ hidden |> viewModLayers
                , hidden |> viewModNeurons gutter
                ]
            , div
                [ class "nodes-wrapper relative"
                , style [ "margin-top" => px 30 ]
                ]
                [ entry |> viewEntryLayer
                , hidden |> viewHiddenLayers gutter
                ]
            ]


output : Model -> Html Msg
output model =
    div
        [ class "ml2" ]
        [ canvas
            [ id "output"
            , class "absolute"
            , style
                [ "width" => toString geometry.outputBox
                , "height" => toString geometry.outputBox
                ]
            ]
            []
        , SvgViews.largeChart geometry.outputBox model.inputs
        ]


viewModLayers : List Layer -> Html Msg
viewModLayers layers =
    div
        [ style
            [ "display" => "flex"
            , "justify-content" => "center"
            ]
        ]
        [ button
            [ class "btn regular"
            , onClick (AddLayer)
            ]
            [ i [ class "fa fa-plus mr1" ] [] ]
        , button
            [ class "btn regular"
            , onClick (RemoveLayer)
            ]
            [ i [ class "fa fa-minus mr1" ] [] ]
        , p [ class "regular" ] [ text (toString (List.length layers) ++ " HIDDEN LAYERS") ]
        ]


viewModNeurons : (Int -> Int) -> List Layer -> Html Msg
viewModNeurons gutter layers =
    let
        buttonClass =
            "btn circle" |> class

        buttonStyle =
            square 20
                ++ [ "padding" => px 0
                   , "margin-left" => px 7
                   , "background-color" => "grey"
                   ]
                |> style

        buttonFaMsg faClass msg =
            button
                [ buttonClass
                , buttonStyle
                , onClick msg
                ]
                [ i [ class <| "fa " ++ faClass, style [ "padding-top" => px 1 ] ] [] ]

        numLayers =
            List.length layers

        spacer x =
            (gutter x) - geometry.boxSize - 5

        layerControls x =
            div
                [ style
                    ([ "position" => "absolute" ]
                        ++ position ( spacer x, 0 )
                    )
                ]
                [ buttonFaMsg "fa-plus" (AddNeuron x)
                , buttonFaMsg "fa-minus" (RemoveNeuron x)
                ]
    in
        div
            [ style [ "position" => "relative" ] ]
            (List.map layerControls [1..(numLayers)])


viewEntryLayer : Layer -> Html Msg
viewEntryLayer inputs =
    div
        [ id "entry-layer" ]
        (List.indexedMap (viewNeuron 0) inputs)


viewHiddenLayers : (Int -> Int) -> List Layer -> Html Msg
viewHiddenLayers gutter hiddenLayers =
    let
        numLayers =
            List.length hiddenLayers

        viewHiddenLayer columnIndex hiddenLayer =
            div
                [ class "hidden-layer"
                , id ("hidden-" ++ (toString columnIndex))
                ]
                (List.indexedMap (spacedNeuron columnIndex) hiddenLayer)

        spacedNeuron columnIndex =
            viewNeuron (gutter (columnIndex + 1))
    in
        div
            [ id "hidden-layers" ]
            (List.indexedMap viewHiddenLayer hiddenLayers)


viewNeuron : Int -> Int -> Neuron -> Html Msg
viewNeuron x y neuron =
    let
        ( dx, dy ) =
            ( x, y * geometry.vertical )
    in
        div
            [ id neuron.id
            , class "absolute border rounded"
            , style
                ([ "color" => "green"
                 , "font-size" => "xx-small"
                 ]
                    ++ square geometry.boxSize
                    ++ position ( dx, dy )
                )
            ]
            [ neuron.weights
                |> List.map (toString >> String.left 6)
                |> String.join " "
                |> Html.text
            ]


px : Int -> String
px x =
    (toString x) ++ "px"


position : ( Int, Int ) -> List ( String, String )
position ( x, y ) =
    [ ( "left", (px x) ), ( "top", (px y) ) ]


square : Int -> List ( String, String )
square w =
    [ ( "width", px w ), ( "height", px w ) ]
