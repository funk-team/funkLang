module Interface.Ui exposing (..)

import Element
import Interface.Model
import Ui.Boxicons
import Ui.Component


pointerToIcon : Interface.Model.InterfacePointer -> Element.Element msg
pointerToIcon { interfaceId } =
    case interfaceId of
        Interface.Model.TransformationKey _ ->
            Ui.Boxicons.bxCodeAlt
                |> Ui.Component.icon

        Interface.Model.ApiCallKey _ ->
            Ui.Boxicons.bxsCircle
                |> Ui.Component.icon


pointerToLabel : Interface.Model.InterfacePointer -> String
pointerToLabel { interfaceId } =
    case interfaceId of
        Interface.Model.TransformationKey _ ->
            "Code"

        Interface.Model.ApiCallKey _ ->
            "API"
