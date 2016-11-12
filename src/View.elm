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
                [ column datasetsWidth dataSets
                , column networkWidth <| network (factor networkWidth) model
                , column outputWidth <| output model
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


network : Int -> Model -> Html Msg
network networkWidth model =
    let
        network =
            model.network

        gutter =
            (*) <| (networkWidth - geometry.boxSize - 20) // (List.length network.hidden)
    in
        div
            [ class "network-wrapper" ]
            [ div
                [ class "layer-editor-wrapper"
                , style [ ( "margin-left", px geometry.boxSize ) ]
                ]
                [ viewModLayers network.hidden
                , viewModNeurons gutter network.hidden
                ]
            , div
                [ class "nodes-wrapper relative"
                , style [ ( "margin-top", px 30 ) ]
                ]
                [ viewEntryLayer network.entry
                , viewHiddenLayers gutter network.hidden
                ]
            ]


output : Model -> Html Msg
output model =
    div
        [ class "ml2" ]
        [ h1 [] [ text "hey there" ]
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
                [ buttonFaMsg "fa-plus" (AddNeuron (x - 1))
                , buttonFaMsg "fa-minus" (RemoveNeuron (x - 1))
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
