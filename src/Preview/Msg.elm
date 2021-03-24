module Preview.Msg exposing (..)

import Action
import Persistence
import ScrollTo
import Spec.Model


type Msg
    = NoOp
    | SpecUpdated ( Persistence.ProjectMeta, Spec.Model.Spec )
    | DispatchUpdate Action.UpdateModelParams
    | ScrollToMsg ScrollTo.Msg
    | DispatchApiCall Action.ApiCallParams
