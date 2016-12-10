module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (lazy2)
import Html.Events exposing (onClick)
import Html.Attributes exposing (attribute, class, id, style)
import List.Extra
import Datasets exposing (Dataset(..))
import Models exposing (..)
import Network exposing (..)
import Update exposing (Msg(..), BackAction(..))
import SvgViews
import Svg
import Svg.Events
import Svg.Attributes exposing (d, stroke, strokeWidth)
import Datasets exposing (xorData, gaussData, circleData)
import Core exposing (colors)
import Polymer.Paper
import String


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type alias Geometry =
    { vertical : Int
    , boxSize : Int
    , datasetsPcnt : Float
    , networkPcnt : Float
    , jumbo : Int
    , networkMarginRight : Int
    }


geometry : Geometry
geometry =
    { vertical = 75
    , boxSize = 40
    , datasetsPcnt = 0.15
    , networkPcnt = 0.65
    , jumbo = 200
    , networkMarginRight = 100
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
            div [ class "col", style [ "width" => (sz |> factor |> px) ] ] [ node ]

        ( datasetsWidth, networkWidth, outputWidth ) =
            ( geometry.datasetsPcnt, geometry.networkPcnt, (1 - (geometry.datasetsPcnt + geometry.networkPcnt)) )
    in
        div
            [ class "main-wrapper" ]
            [ header
            , controls maxWidth model
            , div [ class "ui-wrapper clearfix mx-auto", style [ "width" => px maxWidth ] ]
                [ div [ class "visuals", style [ "margin-top" => "20px" ] ]
                    [ column datasetsWidth <| metaConfig model
                    , div [ class "nodes clearfix", style [ "margin-left" => (datasetsWidth |> factor |> px) ] ]
                        [ column networkWidth <| networkView model (factor networkWidth)
                        , column outputWidth <| output model (factor outputWidth)
                        ]
                    ]
                ]
            ]


metaConfig : Model -> Html Msg
metaConfig model =
    div []
        [ title "DATA"
        , smallText "Select dataset"
        , dataSets model
        ]


header : Html Msg
header =
    div
        [ class "clearfix white bg-black"
        ]
        [ div
            [ class "left p2" ]
            [ Html.text "Elm Brain" ]
        ]


controls : Int -> Model -> Html Msg
controls maxWidth model =
    let
        buttonConfig =
            if model.state == Paused then
                { msg = Begin
                , text = "Go"
                , bkgrnd = "#4caf50"
                }
            else
                { msg = Pause
                , text = "Pause"
                , bkgrnd = "#f44336"
                }

        toggleButton =
            Polymer.Paper.button
                [ attribute "raised" "raised"
                , attribute "toggles" "toggles"
                , onClick buttonConfig.msg
                , style
                    [ "background" => buttonConfig.bkgrnd
                    , "color" => "white"
                    ]
                ]
                [ text buttonConfig.text ]

        reset =
            Polymer.Paper.button
                [ attribute "raised" "raised"
                , style [ "background" => "#e8e8e8" ]
                , onClick Reset
                ]
                [ i [ class "fa fa-refresh", style [ "padding-right" => "7px" ] ] [], text "Reset" ]

        infoStyle =
            [ "margin-left" => px 20
            , "font-size" => "larger"
            , "font-weight" => "100"
            ]

        ticker =
            span
                [ style infoStyle ]
                [ "Epochs: " ++ toString model.nTicks |> text ]

        best =
            span
                [ style <|
                    infoStyle
                        ++ [ "color" => "green" ]
                        ++ (if model.best == 0 then
                                [ "display" => "none" ]
                            else
                                []
                           )
                ]
                [ "Best: " ++ toString model.best |> text ]

        learningRate =
            Polymer.Paper.dropdown
                [ attribute "label" "Learning rate"
                , attribute "noink" "noink"
                , attribute "no-animations" "no-animations"
                , style [ "float" => "right" ]
                ]
                [ Polymer.Paper.listbox
                    [ class "dropdown-content"
                    , attribute "selected" "2"
                    ]
                    (List.map
                        (\a -> Polymer.Paper.item [ onClick <| SetLearningRate a ] [ a |> toString |> text ])
                        Core.learningRates
                    )
                ]
    in
        div
            [ class "controls"
            , style
                [ "padding" => "30px"
                , "background" => "white"
                , "border-bottom" => "1px solid #ddd"
                , "box-shadow" => "0 1px 4px rgba(0,0,0,0.08)"
                ]
            ]
            [ div [ class "clearfix mx-auto", style [ "width" => px maxWidth ] ]
                [ reset
                , toggleButton
                , ticker
                , best
                , learningRate
                ]
            ]


dataSets : Model -> Html Msg
dataSets model =
    let
        dataSelector ( kind, name ) =
            Polymer.Paper.item
                [ kind
                    |> SetDataMode
                    |> onClick
                ]
                [ text name ]

        dataOptions =
            [ ( Stock XOR, "XOR" ), ( Stock Gauss, "Linear" ), ( Stock Circle, "Circle" ), ( Custom, "Custom" ) ]
    in
        div
            [ class "datasets-wrapper"
            , shadow
                :: [ "margin-right" => px 30
                   , "margin-top" => px 0
                   , "cursor" => "pointer"
                   ]
                |> style
            ]
            [ Polymer.Paper.listbox
                [ attribute "selected" "0" ]
                (List.map dataSelector dataOptions)
            ]


networkView : Model -> Int -> Html Msg
networkView model networkWidth =
    let
        network =
            model.network

        gutter =
            networkWidth |> (+) -geometry.networkMarginRight |> (+) -(geometry.boxSize + 20) |> (flip (//)) (List.length network.layers) |> (*)
    in
        div
            [ class "network-wrapper"
            ]
            [ div
                [ class "layer-editor-wrapper"
                , style [ "margin-left" => px geometry.boxSize ]
                ]
                [ network.layers |> viewModLayers
                , network.layers |> viewModNeurons gutter
                ]
            , div
                [ class "nodes-wrapper relative"
                , style [ "margin-top" => px 30 ]
                ]
                [ viewEntryLayer network.entryNeurons
                , viewHiddenLayers gutter network.layers
                , viewLinks gutter networkWidth network
                , hovercard model
                ]
            ]


hovercard : Model -> Html Msg
hovercard model =
    let
        display =
            if model.hoverCard.visible then
                "block"
            else
                "none"
    in
        div
            [ id "hovercard"
            , style
                [ "position" => "absolute"
                , "display" => display
                , "left" => px (model.hoverCard.x + 10)
                , "top" => px (model.hoverCard.y - 10)
                , "background" => "white"
                , "border-radius" => "5px"
                , "border" => "1px solid #aaa"
                , "cursor" => "default"
                , "z-index" => "1000"
                , "padding" => "5px"
                ]
            ]
            [ model.hoverCard.weight |> toString |> String.left 5 |> (++) "Weight: " |> text ]


output : Model -> Int -> Html Msg
output model w =
    let
        scale x a =
            a |> toFloat |> (*) x |> truncate

        errorText er =
            (if er == 0 then
                ""
             else if er < 0.001 then
                " < 0.001"
             else
                toString er
                    |> String.left 5
            )
                |> (++) "Error: "
                |> text

        error =
            div
                [ style
                    [ "margin-bottom" => "10px"
                    ]
                ]
                [ errorText model.network.error ]

        customDisplay =
            if model.dataMode == Custom then
                [ "display" => "flex" ]
            else
                [ "display" => "none" ]

        swatchDim =
            15

        backer icon msg =
            i
                [ class <| "fa fa-" ++ icon
                , onClick msg
                , style
                    [ "cursor" => "pointer"
                    , "margin-right" => px swatchDim
                    ]
                ]
                []

        swatchBorder brush =
            if brush == model.brush then
                [ "border" => "2px solid black" ]
            else
                []

        swatch color brush margin =
            span
                [ style <|
                    swatchBorder brush
                        ++ [ "width" => px swatchDim
                           , "height" => px swatchDim
                           , "background" => color
                           , "margin-left" => margin
                           , "cursor" => "pointer"
                           ]
                , onClick (SetBrush brush)
                ]
                []

        paintControls =
            div [ style (customDisplay ++ [ "height" => px swatchDim ]) ]
                [ backer "angle-double-left" (CustomDataBack All)
                , backer "angle-left" (CustomDataBack Step)
                , swatch colors.negative Negative "auto"
                , swatch colors.positive Positive "10px"
                ]
    in
        div
            []
            [ title "OUTPUT"
            , error
            , canvas
                [ id "output"
                , class "absolute"
                , Html.Attributes.width <| Core.density
                , Html.Attributes.height <| Core.density
                , style
                    [ "width" => px w
                    , "height" => px w
                    ]
                ]
                []
            , SvgViews.largeChart w model
            , paintControls
            ]


viewModLayers : List Layer -> Html Msg
viewModLayers layers =
    let
        layerTitle =
            "HIDDEN LAYER"
                ++ if ((List.length layers) /= 1) then
                    "S"
                   else
                    ""
    in
        div
            [ style
                [ "display" => "flex"
                , "justify-content" => "center"
                , "padding-bottom" => "15px"
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
            , title (toString (List.length layers) ++ " " ++ layerTitle)
            ]


viewModNeurons : (Int -> Int) -> List Layer -> Html Msg
viewModNeurons gutter layers =
    let
        buttonClass =
            "btn circle" |> class

        buttonStyle =
            square 20
                ++ [ "padding" => px 0
                   , "margin-left" => px 4
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
            canvas
                [ id entryConfig.neuron.id
                , onClick <| ToggleEntry entryConfig.kind
                , class "absolute border"
                , Html.Attributes.width <| Core.density
                , Html.Attributes.height <| Core.density
                , style
                    (square geometry.boxSize
                        ++ position ( 0, dy y )
                        ++ activeStyle entryConfig
                        ++ neuronShadowBorder
                        ++ [ "cursor" => "pointer" ]
                    )
                ]
                []

        entryLabels layer =
            layer
                |> List.indexedMap
                    (\y n ->
                        div
                            [ class "entry-label absolute"
                            , style <|
                                (position ( 0, -18 + dy y ))
                                    ++ [ "font-size" => "small" ]
                            ]
                            [ text n.name ]
                    )
    in
        div
            [ id "entry-layer" ]
            ((entryLabels entryLayer)
                ++ (List.indexedMap (viewEntryNeuron) entryLayer)
            )


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


viewLinks : (Int -> Int) -> Int -> Network -> Html Msg
viewLinks gutter maxWidth network =
    let
        layers =
            network.layers

        entryConfig =
            network.entryNeurons

        height =
            List.length entryConfig |> (*) geometry.vertical |> px

        scaleX columnIndex =
            gutter columnIndex

        moveFrom x y =
            (x |> scaleX |> (+) geometry.boxSize |> toString) ++ "," ++ (y * geometry.vertical + (geometry.boxSize // 2) |> toString)

        moveTo x y =
            let
                ( hor, vert ) =
                    if x > List.length layers then
                        ( maxWidth, y )
                    else
                        ( x |> scaleX, y )
            in
                (hor |> toString) ++ "," ++ (vert * geometry.vertical + (geometry.boxSize // 2) |> toString)

        dString x start stop =
            "M" ++ (moveFrom x start) ++ " " ++ (moveTo (x + 1) stop)

        linkColor w =
            stroke <|
                if w < 0 then
                    colors.negative
                else if w > 0 then
                    colors.positive
                else
                    "#fafafa"

        linkWidth w =
            w |> abs |> (*) 2 |> toString |> strokeWidth

        path w x left right =
            Svg.path
                [ dString x left right |> d
                , linkColor w
                , linkWidth w
                ]
                []

        invisiPath w x left right =
            Svg.path
                [ Svg.Events.on "mouseover" (Update.hoverCardPositioner w)
                , Svg.Events.onMouseOut HideHoverCard
                , dString x left right |> d
                , style
                    [ "opacity" => "0"
                    , "stroke-width" => "8"
                    , "stroke" => "black"
                    ]
                ]
                []

        doublePath w x left right =
            [ path w x left right, invisiPath w x left right ]

        hidden =
            layers
                |> List.drop 1
                |> List.indexedMap (\x -> List.indexedMap (\y -> .weights >> List.drop 1 >> List.indexedMap (\i w -> doublePath w (x + 1) i y)))
                |> List.concat
                |> List.concat

        entryXs =
            entryConfig
                |> List.indexedMap (,)
                |> List.filter (Tuple.second >> .active)
                |> List.map Tuple.first

        entry =
            layers
                |> List.take 1
                |> List.concat
                |> List.indexedMap (\i n -> n.weights |> List.map2 (\j w -> doublePath w 0 j i) entryXs)
                |> List.concat

        output =
            case layers |> List.concat |> List.Extra.last of
                Just finalLayer ->
                    network.outputNeuron.weights
                        |> List.drop 1
                        |> List.indexedMap (\i w -> doublePath w (List.length network.layers) i 1)

                Nothing ->
                    network.outputNeuron.weights
                        |> List.drop 1
                        |> List.map2 (\i w -> doublePath w 0 i 1) entryXs
    in
        Svg.svg
            [ style [ "width" => "100%", "height" => height ]
            ]
            (List.concat (entry ++ hidden ++ output))


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
                        ++ neuronShadowBorder
                    )
                ]
                []
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


shadow : ( String, String )
shadow =
    "box-shadow" => "0 2px 2px 0 rgba(0, 0, 0, 0.14),0 1px 5px 0 rgba(0, 0, 0, 0.12),0 3px 1px -2px rgba(0, 0, 0, 0.2)"


title : String -> Html Msg
title txt =
    h3 [ style [ "font-weight" => "100" ] ] [ text txt ]


smallText : String -> Html Msg
smallText txt =
    p [ style [ "font-size" => "small", "color" => "#3e3e3e" ] ] [ text txt ]


neuronShadowBorder : List ( String, String )
neuronShadowBorder =
    shadow :: [ "border-radius" => "3px" ]
