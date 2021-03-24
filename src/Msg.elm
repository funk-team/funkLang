module Msg exposing (Msg(..))

import Canvas.Msg
import Google.Fonts
import Projects.Msg
import ResponsifyTestingEnvironment
import Time


type Msg
    = EditorMsg Canvas.Msg.RootMsg
    | ProjectsMsg Projects.Msg.Msg
    | GotGoogleFonts Google.Fonts.Fonts
    | NoOp
    | SecondPassed Time.Posix
    | ResponsifyTestingEnvironmentMsg ResponsifyTestingEnvironment.Msg
