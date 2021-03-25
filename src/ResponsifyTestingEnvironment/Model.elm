module ResponsifyTestingEnvironment.Model exposing (..)

import Canvas.Tool.Responsify.Model
import RemoteData


type alias TestsData =
    RemoteData.WebData (List ( String, Canvas.Tool.Responsify.Model.ResponsifyExport ))


type InputOutputDisplay
    = DisplayInput
    | DisplayOutput


type alias Model =
    { activeTest : Maybe String
    , tests : TestsData
    , display : InputOutputDisplay
    }


init =
    Model
        Nothing
        RemoteData.NotAsked
        DisplayOutput
