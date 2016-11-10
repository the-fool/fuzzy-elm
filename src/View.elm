module View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import Models exposing (..)
import Update exposing (Msg(..))


type alias Geometry =
    { vertical : Int
    , horizontal : Int
    , boxSize : Int
    , totalWidth : Int
    }


networkGeometry : Geometry
networkGeometry =
    { vertical = 75
    , horizontal = 100
    , boxSize = 60
    , totalWidth = 800
    }


view : Model -> Html Msg
view model =
    div [ class "main-wrapper" ]
        [ header
        , network model.network
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


network : Network -> Html Msg
network layers =
    div
        [ class "network-wrapper relative"
        , style [ ( "top", px 80 ), ( "left", "15%" ), ( "width", px networkGeometry.totalWidth ) ]
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


viewLayerEditor : List Layer -> Html Msg
viewLayerEditor layers =
    div
        [ class "layer-editor-wrapper"
        , style [ ( "margin-left", px networkGeometry.horizontal ) ]
        ]
        [ viewModLayers layers
        , viewModNeurons layers
        ]


viewModLayers : List Layer -> Html Msg
viewModLayers layers =
    div
        [ style [ ( "text-align", "center" ) ]
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
        ]


viewModNeurons : List Layer -> Html Msg
viewModNeurons layers =
    div
        []
        []


viewEntryLayer : Layer -> Html Msg
viewEntryLayer inputs =
    div
        [ id "entry-layer" ]
        (List.indexedMap (viewNeuron 0) inputs)


viewHiddenLayers : List Layer -> Html Msg
viewHiddenLayers hiddenLayers =
    let
        viewHiddenLayer columnIndex hiddenLayer =
            div
                [ class "hidden-layer"
                , id ("hidden-" ++ (toString columnIndex))
                ]
                (List.indexedMap (spacedNeuron columnIndex) hiddenLayer)

        spacedNeuron columnIndex =
            viewNeuron ((columnIndex + 1) * networkGeometry.totalWidth // List.length hiddenLayers)
    in
        div
            [ id "hidden-layers" ]
            (List.indexedMap viewHiddenLayer hiddenLayers)


viewNeuron : Int -> Int -> Neuron -> Html Msg
viewNeuron x y neuron =
    let
        ( dx, dy ) =
            ( x, y * networkGeometry.vertical )
    in
        div
            [ class "absolute border rounded"
            , style (List.concat [ square networkGeometry.boxSize, position ( dx, dy ), [ ( "color", "green" ) ] ])
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
