module Dynamic.Data.Custom exposing (..)

{-| Create custom types similar to elm custom types
-}

import Dict
import Dynamic.Data
import Element
import Element.Input
import IntDict
import Ui.Boxicons
import Ui.Component
import Ui.Table2


{-| Allows the user to edit the state of a type
-}
viewInstanceEditor :
    Dynamic.Data.UnionConstructors
    -> Maybe Dynamic.Data.UnionValue
    -> Bool
    -> Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
viewInstanceEditor variants activeVariant readOnly =
    let
        firstColumn : ( Int, Dynamic.Data.UnionConstructor ) -> Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
        firstColumn ( key, ( variantName, tags ) ) =
            if readOnly then
                Ui.Table2.textCell variantName

            else
                Ui.Table2.input { placeholder = "Tab" ++ String.fromInt (key + 1), text = variantName }
                    |> Element.map (\newVariantName -> Dict.insert key ( newVariantName, tags ) variants)
                    |> Element.map (\newVariants -> ( newVariants, activeVariant ))

        secondColumn : ( Int, Dynamic.Data.UnionConstructor ) -> Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
        secondColumn ( key, ( variantName, tags ) ) =
            if Maybe.map Tuple.first activeVariant == Just key then
                Ui.Table2.icon Ui.Boxicons.bxCheck

            else
                Ui.Table2.button
                    (Element.text
                        (if readOnly then
                            "Use this"

                         else
                            "Set as starting value"
                        )
                    )
                    ( variants, Just ( key, [] ) )

        -- the remove button
        thirdColumn ( key, ( variantName, tags ) ) =
            Ui.Table2.button (Ui.Table2.icon Ui.Boxicons.bxTrash) ( Dict.remove key variants, activeVariant )

        addMsg : ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
        addMsg =
            let
                newVariant : Dynamic.Data.UnionConstructor
                newVariant =
                    ( "", [] )
            in
            ( IntDict.insertNew newVariant variants, activeVariant )

        addVariantButton =
            Element.Input.button
                Ui.Component.buttonStyle
                { label = Element.text "+"
                , onPress = Just addMsg
                }

        columns : List (Element.Column ( Int, Dynamic.Data.UnionConstructor ) ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue ))
        columns =
            [ { width = Element.shrink
              , header = Ui.Table2.headerText "Name"
              , view = firstColumn
              }
            , { width = Element.shrink
              , header =
                    Ui.Table2.headerText
                        (if readOnly then
                            "On click set to"

                         else
                            "Is starting value"
                        )
              , view = secondColumn
              }
            ]
                ++ (if readOnly then
                        []

                    else
                        [ { width = Element.shrink
                          , header = Ui.Table2.headerText ""
                          , view = thirdColumn
                          }
                        ]
                   )

        tableParams =
            { data = variants |> Dict.toList
            , columns = columns
            }

        variantsList : Element.Element ( Dynamic.Data.UnionConstructors, Maybe Dynamic.Data.UnionValue )
        variantsList =
            case Dict.isEmpty variants of
                True ->
                    let
                        label =
                            if readOnly then
                                "No associations yet specified, add them in the model tab"

                            else
                                "Define a variant using the button below"
                    in
                    Element.text label

                False ->
                    Ui.Table2.view tableParams
    in
    Element.column [ Element.width Element.shrink, Element.spacing 10 ]
        [ variantsList
        , if readOnly then
            Element.none

          else
            addVariantButton
        ]
