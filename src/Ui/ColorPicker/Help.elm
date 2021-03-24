module Ui.ColorPicker.Help exposing (..)

import Element
import Element.Border
import Ui.Style


thumbnailAttribs =
    [ Element.width <| Element.px 30
    , Element.height <| Element.px 30
    , Element.Border.width 1
    , Element.Border.color Ui.Style.grey
    , Element.Border.rounded 2

    -- https://stackoverflow.com/questions/26704903/only-in-safari-positionfixed-child-cut-off-when-parent-is-positionfixed-and
    -- safari hides does not allow breaking out of clip on the content but also requires position-fixed on the parent
    -- so we add this class to target the elm-ui onLeft element
    , Ui.Style.class "funk-colorpicker-thumbnail"
    ]
