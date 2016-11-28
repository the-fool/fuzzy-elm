module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (lazy2)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import String
import Models exposing (..)
import Network exposing (..)
import Update exposing (Msg(..))
import SvgViews
import Svg
import Svg.Attributes exposing (d, stroke, strokeWidth)
import Datasets exposing (xorData, gaussData, circleData)
import Core


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type alias Geometry =
    { vertical : Int
    , boxSize : Int
    , datasetsPcnt : Float
    , networkPcnt : Float
    , outputBox : Int
    }


geometry : Geometry
geometry =
    { vertical = 65
    , boxSize = 40
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
        maxWidth =
            Tuple.first model.window |> wrapperWidth

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
                    , column networkWidth <| networkView (factor networkWidth) model.network
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
            if model.state == Paused then
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
            [ ( "XOR", xorData ), ( "GAUSSIAN", gaussData ), ( "CIRCLE", circleData ) ]
    in
        div
            [ class "datasets-wrapper" ]
            (List.map dataSelector dataOptions)


networkView : Int -> Network -> Html Msg
networkView networkWidth network =
    let
        hidden =
            network.layers

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
                [ network.entryNeurons |> viewEntryLayer
                , viewHiddenLayers gutter hidden
                , viewLinks gutter network.entryNeurons hidden
                ]
            ]


output : Model -> Html Msg
output model =
    div
        [ class "ml2" ]
        [ canvas
            [ id "output"
            , class "absolute"
            , Html.Attributes.width <| Core.density
            , Html.Attributes.height <| Core.density
            , style
                [ "width" => px geometry.outputBox
                , "height" => px geometry.outputBox
                ]
            ]
            []
        , lazy2 SvgViews.largeChart geometry.outputBox model.inputs
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
                [ buttonFaMsg "fa-plus" (AddNeuron (x - 1))
                , buttonFaMsg "fa-minus" (RemoveNeuron (x - 1))
                ]
    in
        div
            [ style [ "position" => "relative" ] ]
            (List.map layerControls (List.range 1 (numLayers)))


viewEntryLayer : List EntryNeuron -> Html Msg
viewEntryLayer entryLayer =
    let
        dy y =
            y * geometry.vertical

        activeStyle config =
            if config.active then
                [ "border-width" => "4px" ]
            else
                []

        viewEntryNeuron y entryConfig =
            div
                [ id entryConfig.neuron.id
                , onClick <| ToggleEntry entryConfig.kind
                , class "absolute border rounded"
                , style
                    ([ "color" => "red"
                     , "font-size" => "xx-small"
                     ]
                        ++ square geometry.boxSize
                        ++ position ( 0, dy y )
                        ++ activeStyle entryConfig
                    )
                ]
                [ entryConfig.name
                    |> Html.text
                ]
    in
        div
            [ id "entry-layer" ]
            (List.indexedMap (viewEntryNeuron) entryLayer)


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
            viewNeuron gutter <| columnIndex + 1
    in
        div
            [ id "hidden-layers" ]
            ((List.indexedMap viewHiddenLayer hiddenLayers))


viewLinks : (Int -> Int) -> List EntryNeuron -> List Layer -> Html Msg
viewLinks gutter entryConfig layers =
    let
        height =
            List.length entryConfig |> (*) geometry.vertical |> px

        scaleX columnIndex =
            gutter columnIndex

        moveFrom x y =
            (x |> scaleX |> (+) geometry.boxSize |> toString) ++ "," ++ (y * geometry.vertical + (geometry.boxSize // 2) |> toString)

        moveTo x y =
            (x |> scaleX |> toString) ++ "," ++ (y * geometry.vertical + (geometry.boxSize // 2) |> toString)

        dString x start stop =
            "M" ++ (moveFrom x start) ++ " " ++ (moveTo (x + 1) stop)

        path x start stop =
            Svg.path [ dString x start stop |> d, stroke "#0877bd", strokeWidth "2" ] []

        nonEntry =
            layers
                |> List.drop 1
                |> List.indexedMap (\x -> List.indexedMap (\y -> .weights >> List.drop 1 >> List.indexedMap (\i w -> path (x + 1) i y)))
                |> List.concat
                |> List.concat

        entry =
            entryConfig
                |> List.indexedMap (,)
                |> List.filter (Tuple.second >> .active)
                |> List.map Tuple.first
                |> List.map (\i -> List.indexedMap (\j n -> path 0 i j) (List.take 1 layers |> List.concat))
                |> List.concat
    in
        Svg.svg
            [ style [ "width" => "100%", "height" => height ]
            ]
            (entry ++ nonEntry)


viewNeuron : (Int -> Int) -> Int -> Int -> Neuron -> Html Msg
viewNeuron gutter x y neuron =
    let
        ( dx, dy ) =
            ( gutter x, y * geometry.vertical )
    in
        div []
            [ canvas
                [ id neuron.id
                , class "absolute border"
                , Html.Attributes.width <| Core.density
                , Html.Attributes.height <| Core.density
                , style
                    (square geometry.boxSize
                        ++ position ( dx, dy )
                    )
                ]
                [ neuron.weights
                    |> List.map (toString >> String.left 6)
                    |> String.join " "
                    |> Html.text
                ]
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
