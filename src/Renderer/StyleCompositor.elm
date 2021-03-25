module Renderer.StyleCompositor exposing (RenderedStyles, attribs, batch, compose, noStyle, render)

{-| Sometimes we need to

  - layer differen styles that go into the same attributes like shadow
  - or combine styles that are interdependent like an absolutely rendered rectangle shadowing over layout constraints
    This module provides functions to ensure a cohesive result

-}

import Element
import Ui.Style



-- elm-ui has default styles on input elements which is why we need to noStyle
-- TODO: possibly only apply this when rendering input elements?


noStyle =
    RenderedStyles
        []
        []
        []
        []
        Nothing


attribs =
    RenderedStyles []


type alias RenderedStyles msg =
    { shadows : List String
    , other : List (Element.Attribute msg)
    , background : List (Element.Attribute msg)
    , typoStyles : List (Element.Attribute msg)
    , imageCrop : Maybe (Element.Attribute msg)
    }


compose : RenderedStyles msg -> RenderedStyles msg -> RenderedStyles msg
compose style1 style2 =
    { shadows = style2.shadows ++ style1.shadows
    , other = style2.other ++ style1.other
    , typoStyles = style2.typoStyles ++ style1.typoStyles
    , imageCrop = Maybe.andThen (always style2.imageCrop) style1.imageCrop
    , background = style1.background ++ style2.background
    }


batch : List (Maybe (RenderedStyles msg)) -> RenderedStyles msg
batch =
    List.filterMap identity
        >> List.foldl compose noStyle


render { shadows, other, imageCrop, background, typoStyles } =
    case shadows of
        [] ->
            other
                ++ background
                ++ typoStyles
                ++ (case imageCrop of
                        Nothing ->
                            []

                        Just crop ->
                            [ crop ]
                   )

        some ->
            Ui.Style.shadow some
                :: other
                ++ background
                ++ typoStyles
                ++ (case imageCrop of
                        Nothing ->
                            []

                        Just crop ->
                            [ crop ]
                   )
