module Canvas.AttributesPanel.Content.Shared exposing
    ( imageCropSelector
    , msgFromMaybe
    )

{-| Shared helpers and bits of code
-}

import Canvas.Msg
import Element
import Spec.Element
import Spec.Element.Id
import Spec.Element.Model
import Spec.Element.Render
import Spec.Element.Style
import Spec.Model
import Ui.Dropdown


{-| Remove something if a component returns Nothing
-}
msgFromMaybe id connection_ =
    case connection_ of
        Just connection ->
            Canvas.Msg.MakeDataConnection id connection

        Nothing ->
            Canvas.Msg.RemoveDataConnection id



--- DATA


imageCropSelector : Spec.Element.Model.Element a -> Spec.Model.ElementStyles -> Element.Element Canvas.Msg.Msg
imageCropSelector element elementStyles =
    let
        id =
            element.shared.id

        hasChildren =
            Spec.Element.hasChildren element

        defaultStyles =
            Spec.Element.Style.default Spec.Element.Model.Box

        styles =
            Spec.Element.Id.getFromDict id elementStyles
                |> Maybe.withDefault defaultStyles

        effectivelySelectedOption =
            Spec.Element.Render.effectivelySelectedOption element styles

        viewOption : Spec.Element.Style.ImageCropMode -> Ui.Dropdown.Row Spec.Element.Style.ImageCropMode
        viewOption o =
            let
                isSelected =
                    o == effectivelySelectedOption
            in
            Ui.Dropdown.viewRow
                { label = Ui.Dropdown.Description (label o)
                , onSelect = o
                , isSelected = isSelected
                , rightHandText = Nothing
                , detail = Ui.Dropdown.Description (detail o)
                , sideNote = Ui.Dropdown.NoDetail
                }

        rows =
            List.map
                viewOption
                (if hasChildren then
                    Spec.Element.Style.imageModesCompatibleWithBackgroundOnly

                 else
                    Spec.Element.Style.imageModes
                )

        label o =
            case o of
                Spec.Element.Style.Cover ->
                    "Cover"

                Spec.Element.Style.Contain ->
                    "Contain"

                Spec.Element.Style.AutoWidth ->
                    "Auto Width"

                Spec.Element.Style.AutoHeight ->
                    "Auto Height"

        detail o =
            case o of
                Spec.Element.Style.Cover ->
                    "Image will be clipped if its aspect ratio does not match the frame"

                Spec.Element.Style.Contain ->
                    "Image will be letterboxed (space is added) if aspec ratios are not identical."

                Spec.Element.Style.AutoHeight ->
                    "The box's height will grow to match the aspect ratio of the image"

                Spec.Element.Style.AutoWidth ->
                    "The box's width will grow to match the aspect ratio of the image"
    in
    Ui.Dropdown.view [] { label = label effectivelySelectedOption, contents = rows }
        |> Element.map (Canvas.Msg.SetImageCropMode id)
