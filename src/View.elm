module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Models exposing (..)
import Update exposing (Msg(..))


layerSpacing : { vertical : Int, horizontal : Int }
layerSpacing =
    { vertical = 75
    , horizontal = 100
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
            [ text "Elm Brain" ]
        ]


network : Network -> Html Msg
network layers =
    div
        [ class "network-wrapper"
        , style [ ( "position", "relative" ), ( "top", px 80 ), ( "left", px 30 ) ]
        ]
        [ viewEntryLayer layers.entry
        , viewHiddenLayers layers.hidden
        ]


viewEntryLayer : Layer -> Html Msg
viewEntryLayer inputs =
    div
        [ id "entry-layer" ]
        (List.indexedMap (viewNeuron 0) inputs)


viewHiddenLayers : List Layer -> Html Msg
viewHiddenLayers hiddenLayers =
    div
        [ id "hidden-layers" ]
        (List.indexedMap viewHiddenLayer hiddenLayers)


viewHiddenLayer : Int -> Layer -> Html Msg
viewHiddenLayer column hiddenLayer =
    div
        [ class "hidden-layer"
        , id ("hidden-" ++ (toString column))
        ]
        (List.indexedMap (viewNeuron (column + 1)) hiddenLayer)


viewNeuron : Int -> Int -> Neuron -> Html Msg
viewNeuron x y neuron =
    let
        ( dx, dy ) =
            ( x * layerSpacing.horizontal, y * layerSpacing.vertical )
    in
        div
            [ class "absolute border rounded"
            , style (List.concat [ square 60, position dx dy, [ ( "color", "green" ) ] ])
            ]
            [ text (toString neuron.id)
            , text (toString ( x, y ))
            ]


px : Int -> String
px x =
    (toString x) ++ "px"


position : Int -> Int -> List ( String, String )
position x y =
    [ ( "left", (px x) ), ( "top", (px y) ) ]


square : Int -> List ( String, String )
square w =
    [ ( "width", px w ), ( "height", px w ) ]
