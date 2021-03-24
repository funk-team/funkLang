module Canvas.Tool.Draw.Msg exposing (Msg(..))

import Canvas.Tool.Draw.Model


{-| Elements that are enclosed by another rectangle and need to be moved accordingly in the tree
-}
type
    Msg
    -- start on either
    = ScenePress Canvas.Tool.Draw.Model.OnSceneData
    | ElementPress Canvas.Tool.Draw.Model.OnElementData
      -- move on either
    | SceneMove Canvas.Tool.Draw.Model.OnSceneData
    | ElementMove Canvas.Tool.Draw.Model.OnElementData
      -- realease anywhere :)
    | Release
