module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import Models exposing (..)
import Network exposing (..)
import Update exposing (Msg(..))
import SvgViews exposing (largeChart)
import Datasets exposing (selectXor)


type alias Geometry =
    { vertical : Int
    , boxSize : Int
    , datasetsPcnt : Float
    , networkPcnt : Float
    }


geometry : Geometry
geometry =
    { vertical = 75
    , boxSize = 45
    , datasetsPcnt = 0.1
    , networkPcnt = 0.6
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
            List.take (List.length model.network.layers - 1) model.network.layers

        maxWidth =
            fst model.window |> wrapperWidth

        factor x =
            maxWidth |> toFloat |> (*) x |> truncate

        column sz node =
            div [ class "col", style [ ( "width", factor sz |> px ) ] ] [ node ]

        ( datasetsWidth, networkWidth, outputWidth ) =
            ( geometry.datasetsPcnt, geometry.networkPcnt, (1 - (geometry.datasetsPcnt + geometry.networkPcnt)) )
    in
        div
            [ class "main-wrapper" ]
            [ header
            , div [ class "ui-wrapper clearfix mx-auto", style [ ( "width", px maxWidth ) ] ]
                [ div [ class "controls" ]
                    [ h1 [] [ text "controls" ]
                    , button [ class "btn", onClick (Begin 1) ] []
                    ]
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


dataSets : Model -> Html Msg
dataSets model =
    let
        seeder =
            case List.head model.inputs of
                Nothing ->
                    2

                Just ( x, y, c ) ->
                    (x * y) ^ 2 + 1

        dataSelector ( name, handler ) =
            div [ onClick <| handler seeder ] [ text name ]

        dataOptions =
            [ ( "XOR", selectXor ), ( "GAUSSIAN", selectXor ) ]
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
                    [ [] ]

        hidden =
            List.drop 1 layers

        gutter =
            (*) <| (networkWidth - geometry.boxSize - 20) // (List.length hidden)
    in
        div
            [ class "network-wrapper" ]
            [ div
                [ class "layer-editor-wrapper"
                , style [ ( "margin-left", px geometry.boxSize ) ]
                ]
                [ hidden |> viewModLayers
                , hidden |> viewModNeurons gutter
                ]
            , div
                [ class "nodes-wrapper relative"
                , style [ ( "margin-top", px 30 ) ]
                ]
                [ entry |> viewEntryLayer
                , hidden |> viewHiddenLayers gutter
                ]
            ]


output : Model -> Html Msg
output model =
    div
        [ class "ml2" ]
        [ largeChart 300 model.inputs ]


viewModLayers : List Layer -> Html Msg
viewModLayers layers =
    div
        [ style [ ( "display", "flex" ), ( "justify-content", "center" ) ]
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
            List.concat
                [ square 20
                , [ ( "padding", px 0 )
                  , ( "margin-left", px 7 )
                  , ( "background-color", "grey" )
                  ]
                ]
                |> style

        buttonFaMsg faClass msg =
            button
                [ buttonClass
                , buttonStyle
                , onClick msg
                ]
                [ i [ class <| "fa " ++ faClass, style [ ( "padding-top", px 1 ) ] ] [] ]

        numLayers =
            List.length layers

        spacer x =
            (gutter x) - geometry.boxSize - 5

        layerControls x =
            div
                [ style (List.concat [ [ ( "position", "absolute" ) ], position ( spacer x, 0 ) ])
                ]
                [ buttonFaMsg "fa-plus" (AddNeuron x)
                , buttonFaMsg "fa-minus" (RemoveNeuron x)
                ]
    in
        div
            [ style [ ( "position", "relative" ) ] ]
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
            [ class "absolute border rounded"
            , style (List.concat [ square geometry.boxSize, position ( dx, dy ), [ ( "color", "green" ) ] ])
            ]
            [ Html.text (toString neuron)
            , Html.text (toString ( x, y ))
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
