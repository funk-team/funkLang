module DesignSystem.Msg exposing (Msg(..))

import DesignSystem.Color
import DesignSystem.Color.Model
import DesignSystem.IconBrowser.Msg
import DesignSystem.Menus
import DesignSystem.Shadow
import DesignSystem.Typography


type Msg
    = GotColorEditorMsg DesignSystem.Color.Msg
    | GotIconBrowserMsg DesignSystem.IconBrowser.Msg.Msg
    | GotShadowMsg ( DesignSystem.Shadow.Msg, DesignSystem.Color.Model.Model )
    | GotTypoMsg ( DesignSystem.Typography.Msg, DesignSystem.Color.Model.Model )
    | SwitchMenu DesignSystem.Menus.Menu
