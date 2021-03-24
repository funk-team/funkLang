module Preview.Model exposing (Model, init)

{-| @@TODO: use opaque type to ensure proper initialization
-}

import Dynamic.Data
import IntDict


init =
    IntDict.empty


type alias Model =
    IntDict.Dict Dynamic.Data.Instance
