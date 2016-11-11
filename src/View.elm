module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import Models exposing (..)
import Update exposing (Msg(..))
import SvgViews exposing (largeChart)
import SvgViews exposing (randomPairs)


type alias Geometry =
    { vertical : Int
    , horizontal : Int
    , boxSize : Int
    , totalWidth : Int
    , padding : Int
    }


geometry : Geometry
geometry =
    { vertical = 75
    , horizontal = 100
    , boxSize = 45
    , totalWidth = 700
    , padding = 20
    }


view : Model -> Html Msg
view model =
    let
        col =
            toString >> (++) "col col-"

        column sz node =
            div [ class <| col sz ] [ node ]
    in
        div [ class "main-wrapper" ]
            [ header
            , div [ class "ui-wrapper clearfix m3" ]
                [ column 2 dataSets
                , column 6 <| network model.network
                , column 4 <| output model
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


dataSets : Html Msg
dataSets =
    let
        data =
            List.map (\( x, y ) -> ( x, y, 1 )) <| randomPairs 100
    in
        div
            [ class "datasets-wrapper"
            ]
            [ largeChart 100 data ]


network : Network -> Html Msg
network layers =
    div
        [ class "network-wrapper"
        ]
        [ viewLayerEditor layers.hidden
        , div
            [ class "nodes-wrapper relative"
            , style [ ( "margin-top", px 30 ) ]
            ]
            [ viewEntryLayer layers.entry
            , viewHiddenLayers layers.hidden
            ]
        ]


output : Model -> Html Msg
output model =
    div
        []
        [ h1 [] [ text "hey there" ]
        ]


gutter : Int -> Int -> Int
gutter nlayers factor =
    ((geometry.totalWidth - geometry.boxSize) // nlayers) * factor


viewLayerEditor : List Layer -> Html Msg
viewLayerEditor layers =
    let
        leftMargin =
            geometry.boxSize
                |> px

        editorStyle =
            [ ( "margin-left", leftMargin ) ]
                |> style
    in
        div
            [ class "layer-editor-wrapper"
            , editorStyle
            ]
            [ viewModLayers layers
            , viewModNeurons layers
            ]


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


viewModNeurons : List Layer -> Html Msg
viewModNeurons layers =
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

        numLayers =
            List.length layers

        spacer x =
            (gutter numLayers x) - geometry.boxSize - 5

        layerControls x =
            div
                [ style (List.concat [ [ ( "position", "absolute" ) ], position ( spacer x, 0 ) ])
                ]
                [ button
                    [ buttonClass
                    , buttonStyle
                    , onClick (AddNeuron <| x - 1)
                    ]
                    [ i [ class "fa fa-plus", style [ ( "padding-top", px 1 ) ] ] [] ]
                , button
                    [ buttonClass
                    , buttonStyle
                    , onClick (RemoveNeuron <| x - 1)
                    ]
                    [ i [ class "fa fa-minus", style [ ( "padding-top", px 1 ) ] ] [] ]
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


viewHiddenLayers : List Layer -> Html Msg
viewHiddenLayers hiddenLayers =
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
            viewNeuron (gutter numLayers (columnIndex + 1))
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
            [ Html.text (toString neuron.id)
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
