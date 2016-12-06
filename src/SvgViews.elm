module SvgViews exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (on)
import Core
import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Update exposing (mouseEventDecoder, Msg(..))
import Models exposing (..)


paintBrusher : Int -> Decoder Msg
paintBrusher maxRange =
    let
        scaler =
            Core.scale ( 0, toFloat maxRange ) ( -Core.dataRange, Core.dataRange )

        scale { x, y } =
            ( scaler (toFloat x), scaler (toFloat y) )
    in
        mouseEventDecoder
            (\coord -> AddPoint (scale coord))


largeChart : Int -> Model -> Html.Html Update.Msg
largeChart dim model =
    let
        data =
            model.inputs

        factor =
            (+) Core.dataRange >> (*) (toFloat dim / (Core.dataRange * 2))

        normalizedData =
            Array.map (\datum -> { datum | coord = ( Tuple.first datum.coord |> factor, Tuple.second datum.coord |> factor ) }) data
                |> Array.toList

        s =
            toString

        fillColor label =
            if label == 1 then
                Core.colors |> .positive
            else
                Core.colors |> .negative

        styles =
            ((if model.dataMode == Custom then
                [ ( "cursor", "pointer" ) ]
              else
                [ ( "cursor", "normal" ) ]
             )
                ++ [ ( "position", "relative" ) ]
            )
                |> Html.Attributes.style

        events =
            if model.dataMode == Custom then
                [ Svg.Events.on "mousdown" (paintBrusher dim) ]
            else
                []

        toCircle { coord, label } =
            circle [ cx (Tuple.first coord |> s), cy (Tuple.second coord |> s), r (s 3), stroke "white", strokeWidth "1", fillColor label |> fill ] []
    in
        svg
            ([ version "1.1"
             , x "0"
             , y "0"
             , width <| toString dim
             , height <| toString dim
             , styles
             , Svg.Events.on "mousedown" (paintBrusher dim)
             ]
            )
            (List.map toCircle normalizedData)
