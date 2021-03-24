module Ui.Layout exposing (..)

import Element
import Ui.Style


viewLeftSidebar : List (Element.Element msg) -> Element.Element msg
viewLeftSidebar children =
    Element.column
        [ Element.width (Element.px Ui.Style.leftSidebarWidth), Element.alignTop, Element.moveDown 100 ]
        children


viewRightSidebar : Element.Element msg
viewRightSidebar =
    Element.el [ Element.width (Element.px Ui.Style.rightSidebarWidth) ] Element.none
