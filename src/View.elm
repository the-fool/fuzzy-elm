module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Models exposing (..)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ class "main-wrapper" ]
        [ network model.network ]


network : Network -> Html Msg
network layers =
    div
        [ class "network-wrapper"
        ]
        [ viewEntryLayer layers.entry
        , viewHiddenLayers layers.hidden
        ]


viewEntryLayer : Layer -> Html Msg
viewEntryLayer inputs =
    div
        [ id "entry-layer" ]
        (List.map viewNeuron inputs)


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
        (List.map viewNeuron hiddenLayer)


viewNeuron : Neuron -> Html Msg
viewNeuron neuron =
    text (toString neuron.id)
