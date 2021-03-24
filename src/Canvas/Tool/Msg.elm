module Canvas.Tool.Msg exposing (..)

import Canvas.Tool.Cut
import Canvas.Tool.Debug
import Canvas.Tool.Draw.Msg
import Canvas.Tool.Transform.Msg


type Msg
    = DrawMsg Canvas.Tool.Draw.Msg.Msg
    | CutMsg Canvas.Tool.Cut.Msg
    | TransformMsg Canvas.Tool.Transform.Msg.Msg
    | DebugMsg Canvas.Tool.Debug.Msg
