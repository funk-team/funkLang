module DeployEditor.Msg exposing (Msg(..))

import DeployEditor.Deploy
import DeployEditor.Domain
import DeployEditor.Menus


type Msg
    = SwitchMenu DeployEditor.Menus.Menu
    | GotDeployEditorMsg DeployEditor.Deploy.Msg
    | GotDomainEditorMsg DeployEditor.Domain.Msg
