module Projects.Msg exposing (..)

import Projects.Model
import RemoteData


type Msg
    = NoOp
    | CreateProjectButtonClicked
    | ProjectCreated (RemoteData.WebData Projects.Model.Project)
    | ProjectsReceived (RemoteData.WebData (List Projects.Model.Project))
    | DeleteClicked String
    | ProjectDeleted String (RemoteData.WebData String)
    | ProjectUpdated String (RemoteData.WebData Projects.Model.Project)
    | DeleteConfirmClicked
    | DeleteAborted
    | Renamed String String
    | BounceMsg
    | UpdateOnBoardingScreen Int
    | ToggleOnBoardingScreen
