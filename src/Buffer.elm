module Buffer exposing (..)

import Native.Buffer


type Buffer
    = Buffer


buffer : Int -> Buffer
buffer =
    Native.Buffer.buffer


set : Int -> a -> Buffer -> Buffer
set =
    Native.Buffer.set


get : Int -> Buffer -> a
get =
    Native.Buffer.get
