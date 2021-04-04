module Generator exposing
    ( dummyRange
    , renderCode
    , renderFile
    , renderFiles
    , renderScreen
    , write
    )

import Canvas.Events
import Color
import DesignSystem
import DesignSystem.Color.Model
import DesignSystem.Color.Selection
import DesignSystem.Shadow
import DesignSystem.Typography
import Dict
import Element
import Element.Background
import Element.Input
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Writer
import Http
import Interface.Scope
import Json.Encode as Encode
import Model.Model
import Rectangle
import Spec
import Spec.Element.Id
import Spec.Element.Layout
import Spec.Element.Layout.Padding
import Spec.Element.Model
import Spec.Element.Style
import Spec.Element.Style.Edges
import Spec.Model
import String.Case
import Ui.ColorPicker.Gradient
import Ui.Generated
import Ui.Style


renderCode : msg -> Model.Model.UserModel -> Element.Element msg
renderCode msg userModel =
    let
        code =
            renderFile userModel
                |> Element.text

        button =
            Element.Input.button
                Ui.Generated.el527879678
                { label = Element.text "Write to file", onPress = Just msg }
    in
    Element.column
        [ Element.padding 10
        , Element.spacing 10
        , Element.Background.color Ui.Style.white
        , Element.alignBottom
        ]
        [ code
        , button
        ]


{-| Render an elm source code file

Elm source code files have a specific structure: module decl, imports, everything else

-}
renderFile : Model.Model.UserModel -> String
renderFile ({ itemsOnCanvas, elementStyles } as userModel) =
    ""


write : (Result Http.Error () -> msg) -> Model.Model.UserModel -> Cmd msg
write msg userModel =
    let
        txt =
            renderFile userModel

        json =
            Encode.string txt
    in
    Http.post
        { url = "/api/generated-writer"
        , body = Http.jsonBody json
        , expect = Http.expectWhatever msg
        }


viewModule =
    [ "Generated", "View" ]


designSystemModule =
    [ "Generated", "DesignSystem" ]


renderFiles : Model.Model.UserModel -> List { name : String, content : String }
renderFiles userModel =
    let
        toString =
            List.intersperse "/"
                >> (\l -> l ++ [ ".elm" ])
                >> List.foldr (++) ""
    in
    [ { name = toString viewModule, content = renderViewsAsOneFile userModel }
    , { name = toString designSystemModule, content = renderDesignSystemFile userModel.designSystem }
    ]


renderViewsAsOneFile : Model.Model.UserModel -> String
renderViewsAsOneFile userModel =
    let
        {- all the contents. here render only one element, the one that is selected -}
        declarations =
            [ node declareViewScreens ]

        declareViewScreens =
            List.map (renderScreen userModel) userModel.itemsOnCanvas
                |> List.map node
                |> Elm.Syntax.Expression.ListExpr
                |> renderDeclaration "viewScreens"

        moduleDef : Elm.Syntax.Module.Module
        moduleDef =
            Elm.Syntax.Module.NormalModule <|
                Elm.Syntax.Module.DefaultModuleData
                    (node viewModule)
                    (node <| Elm.Syntax.Exposing.All dummyRange)

        imports =
            elementImports
                ++ [ Elm.Syntax.Import.Import (node [ "Html", "Attributes" ]) Nothing Nothing
                   , Elm.Syntax.Import.Import (node [ "Generated", "DesignSystem" ]) Nothing Nothing
                   ]
                |> List.map node

        file =
            Elm.Syntax.File.File
                (node moduleDef)
                imports
                declarations
                []

        renderedFile =
            file
                |> Elm.Writer.writeFile
                |> Elm.Writer.write
    in
    renderedFile


renderDesignSystemFile : DesignSystem.Model -> String
renderDesignSystemFile designSystem =
    let
        {- all the contents. here render only one element, the one that is selected -}
        declarations =
            renderDesignSystem designSystem
                |> List.map node

        moduleDef : Elm.Syntax.Module.Module
        moduleDef =
            Elm.Syntax.Module.NormalModule <|
                Elm.Syntax.Module.DefaultModuleData
                    (node designSystemModule)
                    (node <| Elm.Syntax.Exposing.All dummyRange)

        imports =
            elementImports
                |> List.map node

        file =
            Elm.Syntax.File.File
                (node moduleDef)
                imports
                declarations
                []

        renderedFile =
            file
                |> Elm.Writer.writeFile
                |> Elm.Writer.write
    in
    renderedFile


elementImports =
    [ Elm.Syntax.Import.Import (node [ "Element" ]) Nothing Nothing
    , Elm.Syntax.Import.Import (node [ "Element", "Font" ]) Nothing Nothing
    , Elm.Syntax.Import.Import (node [ "Element", "Border" ]) Nothing Nothing
    , Elm.Syntax.Import.Import (node [ "Element", "Background" ]) Nothing Nothing
    ]



--
{-
   Attributes type is used to
   separate functions that give a list of attributes [1]
   from the other attributes [2]

   ex:

       ([ Element.width Element.fill -- [2]
        , Element.Border.width 5 -- [2]
        ]
           ++ getTypoAttributes -- [1]
           ++ getShadowAttributes -- [1]
       )

    getTypoAttributes : List (Element.Attributes msg)
    getShadowAttributes : List (Element.Attributes msg)
-}


type Attributes
    = Attributes (List Elm.Syntax.Expression.Expression) (List Elm.Syntax.Expression.Expression)


attributesToRecord attr =
    let
        (Attributes attributes functions) =
            attr
    in
    { attributes = attributes, functions = functions }


appendAttributes attr1 attr2 =
    let
        ( a1, a2 ) =
            ( attributesToRecord attr1, attributesToRecord attr2 )
    in
    Attributes (a1.attributes ++ a2.attributes) (a1.functions ++ a2.functions)


mergeFunctionsAndAttributes : Attributes -> List Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
mergeFunctionsAndAttributes attr suplAttr =
    let
        { attributes, functions } =
            attributesToRecord attr

        initialAtribsList =
            attributes
                ++ suplAttr
                |> List.map node
                |> Elm.Syntax.Expression.ListExpr
    in
    List.foldr renderAppendLists initialAtribsList functions
        |> renderParenthesize


renderDeclaration : String -> Elm.Syntax.Expression.Expression -> Elm.Syntax.Declaration.Declaration
renderDeclaration declarationName expression =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            Elm.Syntax.Expression.FunctionImplementation
                (node declarationName)
                []
                (node expression)
    in
    Elm.Syntax.Declaration.FunctionDeclaration
        (Elm.Syntax.Expression.Function Nothing Nothing (node implementation))



--
---- DECLARATION RENDERERS ----


renderDesignSystem : DesignSystem.Model -> List Elm.Syntax.Declaration.Declaration
renderDesignSystem designSystem =
    renderTypos designSystem.typoEditor
        ++ renderColors designSystem.colorEditor
        ++ renderShadows ( designSystem.colorEditor, designSystem.shadows )


renderTypos : DesignSystem.Typography.Model -> List Elm.Syntax.Declaration.Declaration
renderTypos model =
    let
        toMaybeDeclaration ( selection, _ ) =
            case DesignSystem.Typography.selectedTypographyStyle selection model of
                Just style ->
                    renderDeclaration
                        (typoFuncName selection style)
                        (renderTypoStyles style |> List.map node |> Elm.Syntax.Expression.ListExpr)
                        |> Just

                Nothing ->
                    Nothing
    in
    -- DesignSystem.Typography.getTypographySelectionList model
    Dict.toList model.typos
        |> List.filterMap toMaybeDeclaration


renderColors : DesignSystem.Color.Model.Model -> List Elm.Syntax.Declaration.Declaration
renderColors model =
    let
        toMaybeDeclaration selection =
            case DesignSystem.Color.Model.getSelectedSwatch selection model of
                Just swatch ->
                    renderDeclaration
                        (colorFuncName selection swatch)
                        (renderColor swatch.value)
                        |> Just

                Nothing ->
                    Nothing
    in
    DesignSystem.Color.Model.getSelectionList model
        |> List.filterMap toMaybeDeclaration


renderShadows : ( DesignSystem.Color.Model.Model, DesignSystem.Shadow.Model ) -> List Elm.Syntax.Declaration.Declaration
renderShadows ( colorModel, model ) =
    let
        renderedShadowAttributesList shadow =
            renderShadowAttributes colorModel shadow
                |> List.map node
                |> Elm.Syntax.Expression.ListExpr

        toDeclaration ( index, shadow ) =
            renderDeclaration
                (shadowFuncName index shadow)
                (renderedShadowAttributesList shadow)
    in
    Dict.toList model.shadows
        |> List.map toDeclaration



---- FUNC CALLS RENDERERS ----


maybeRenderedDesignSystemColor : DesignSystem.Color.Model.Model -> DesignSystem.Color.Model.Selection -> Maybe Elm.Syntax.Expression.Expression
maybeRenderedDesignSystemColor model selection =
    case DesignSystem.Color.Model.getSelectedSwatch selection model of
        Just swatch ->
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue designSystemModule (colorFuncName selection swatch) ]
                |> Just

        Nothing ->
            Nothing


renderedDesignSystemShadowAttributes : DesignSystem.Shadow.Model -> Int -> List Elm.Syntax.Expression.Expression
renderedDesignSystemShadowAttributes model index =
    case Dict.get index model.shadows of
        Just shadow ->
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue designSystemModule (shadowFuncName index shadow) ]
                |> List.singleton

        Nothing ->
            []



---- HELPERS ----


typoFuncName : Int -> DesignSystem.Typography.Typo -> String
typoFuncName selection style =
    style.name
        ++ String.fromInt selection
        ++ "TypoAttributes"
        |> String.Case.toCamelCaseLower


colorFuncName : DesignSystem.Color.Model.Selection -> DesignSystem.Color.Model.Swatch -> String
colorFuncName selection swatch =
    let
        toString selection_ =
            case selection_ of
                DesignSystem.Color.Model.BackgroundSwatchSelected ->
                    "Bg"

                DesignSystem.Color.Model.TextSwatchSelected ->
                    "Text"

                DesignSystem.Color.Model.OtherSwatchSelected index ->
                    "Other" ++ String.fromInt index
    in
    swatch.label
        ++ toString selection
        ++ "ColorElement"
        |> String.Case.toCamelCaseLower


shadowFuncName : Int -> DesignSystem.Shadow.Shadow -> String
shadowFuncName index shadow =
    shadow.name
        ++ String.fromInt index
        ++ "ShadowAttributes"
        |> String.Case.toCamelCaseLower


renderScreen : Model.Model.UserModel -> Spec.Element.Model.Screen -> Elm.Syntax.Expression.Expression
renderScreen userModel { shared, outerGeometry } =
    let
        { id } =
            shared

        renderedScreenLayoutAttributes =
            [ renderElementFunc "width" <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] "fill"
            , renderElementFunc "height" <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] "fill"
            ]
    in
    sharedRenderer userModel shared renderedScreenLayoutAttributes


renderAbsoluteElement : Model.Model.UserModel -> Spec.Element.Model.AbsoluteElement -> Elm.Syntax.Expression.Expression
renderAbsoluteElement userModel { shared, outerGeometry } =
    let
        { id } =
            shared

        context =
            Interface.Scope.empty

        renderedLayoutAttributes =
            renderAbsoluteLayoutAttributes (Spec.getAbsoluteElementRectangle userModel context outerGeometry)
    in
    sharedRenderer userModel shared renderedLayoutAttributes


renderFlowElement : Model.Model.UserModel -> Spec.Element.Model.FlowElement -> Elm.Syntax.Expression.Expression
renderFlowElement userModel { shared, outerGeometry } =
    let
        { id } =
            shared

        renderedLayoutAttributes =
            renderAlignmentAttributes outerGeometry.alignment
                ++ renderSizeAttributes userModel outerGeometry.size
    in
    sharedRenderer userModel shared renderedLayoutAttributes


sharedRenderer : Model.Model.UserModel -> Spec.Element.Model.Shared -> (List Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression)
sharedRenderer userModel shared =
    \layoutAttributes ->
        let
            { id } =
                shared

            -- attribs
            attributes =
                renderPaddingAttribute shared.padding
                    :: renderSpacingAttribute shared.spacing
                    :: renderIdAttribute id
                    :: layoutAttributes
                    |> mergeFunctionsAndAttributes renderedStyleAttributes

            renderedStyleAttributes =
                Spec.Element.Id.getFromDict id userModel.elementStyles
                    |> Maybe.map (\style -> renderStyleAttributes userModel ( id, style ))
                    |> Maybe.withDefault (Attributes [] [])

            -- children
            childrenDisposition =
                case shared.flow of
                    Spec.Element.Layout.Column ->
                        "column"

                    Spec.Element.Layout.Row ->
                        "row"

                    Spec.Element.Layout.WrappedRow ->
                        "wrappedRow"

            children =
                renderChildren userModel ( id, shared.children )
                    |> List.map node
        in
        Elm.Syntax.Expression.Application
            [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] childrenDisposition
            , node <| attributes
            , node <| Elm.Syntax.Expression.ListExpr children
            ]


renderIdAttribute : Spec.Element.Id.Id -> Elm.Syntax.Expression.Expression
renderIdAttribute id =
    let
        str =
            Spec.Element.Id.toHtmlIdRaw id

        htmlAttribute =
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Html", "Attributes" ] "id"
                , node <| Elm.Syntax.Expression.Literal str
                ]

        elementAttribute =
            renderElementFunc "htmlAttribute" <| renderLeftPipe htmlAttribute
    in
    elementAttribute


renderChildren : Model.Model.UserModel -> ( Spec.Element.Id.Id, Spec.Element.Model.Children ) -> List Elm.Syntax.Expression.Expression
renderChildren userModel ( id, children ) =
    case children of
        Spec.Element.Model.FlowChildren flowChildrenList ->
            List.map
                (\child -> renderFlowElement userModel child)
                flowChildrenList

        Spec.Element.Model.AbsoluteChildren absoluteChildrenList ->
            List.map
                (\child -> renderAbsoluteElement userModel child)
                absoluteChildrenList



--
---- ABSOLUTE ELEMENTS ----


renderAbsoluteLayoutAttributes : Canvas.Events.ElementRectangle -> List Elm.Syntax.Expression.Expression
renderAbsoluteLayoutAttributes (Canvas.Events.ElementRectangle size) =
    let
        ( height, width ) =
            ( Rectangle.height size |> round
            , Rectangle.width size |> round
            )

        ( offsetX, offsetY ) =
            ( Rectangle.x1 size |> round
            , Rectangle.y1 size |> round
            )

        renderWidthOrHeight : String -> (Int -> Elm.Syntax.Expression.Expression)
        renderWidthOrHeight widthOrHeight =
            renderElementFunc widthOrHeight
                << renderLeftPipe
                << renderElementFunc "px"
                << Elm.Syntax.Expression.Integer

        renderMove : String -> (Int -> Elm.Syntax.Expression.Expression)
        renderMove move =
            renderElementFunc move
                << Elm.Syntax.Expression.Integer
    in
    [ renderWidthOrHeight "width" width
    , renderWidthOrHeight "height" height
    , renderMove "moveDown" offsetY
    , renderMove "moveRight" offsetX
    ]



---- IN FLOW ELEMENTS ----


renderSizeAttributes : Spec.Model.WithSpec a -> Spec.Element.Layout.Size -> List Elm.Syntax.Expression.Expression
renderSizeAttributes userModel { width, height } =
    [ renderElementFunc "width" <| renderLeftPipe <| renderLength userModel width
    , renderElementFunc "height" <| renderLeftPipe <| renderLength userModel height
    ]


renderLength : Spec.Model.WithSpec userModel -> Spec.Element.Layout.Length -> Elm.Syntax.Expression.Expression
renderLength userModel { behavior, minMax } =
    let
        scope =
            Interface.Scope.empty

        { min, max } =
            minMax

        renderNullable m value =
            Spec.resolveNullable userModel scope value
                |> Maybe.map round
                |> renderMinOrMax m

        renderedMaximum =
            renderNullable "minimum" min

        renderedMinimum =
            renderNullable "maximum" max

        renderMinOrMax : String -> Maybe Int -> Elm.Syntax.Expression.Expression
        renderMinOrMax minOrMax maybeVal =
            case maybeVal of
                Nothing ->
                    renderedIdentity

                Just 0 ->
                    renderedIdentity

                Just len ->
                    renderElementFunc minOrMax <| Elm.Syntax.Expression.Integer len

        pipeMinAndMax : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
        pipeMinAndMax expr =
            Elm.Syntax.Expression.Application
                [ node <| renderedMinimum
                , node <|
                    renderLeftPipe <|
                        Elm.Syntax.Expression.Application
                            [ node <| renderedMaximum
                            , node <|
                                renderLeftPipe <|
                                    Elm.Syntax.Expression.Application
                                        [ node <| expr ]
                            ]
                ]
    in
    case behavior of
        Spec.Element.Layout.Static px ->
            let
                value =
                    Spec.resolve userModel scope px
                        |> round
            in
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] "px"
                , node <| Elm.Syntax.Expression.Integer value
                ]

        Spec.Element.Layout.Fill ->
            pipeMinAndMax (Elm.Syntax.Expression.FunctionOrValue [ "Element" ] "fill")

        Spec.Element.Layout.Shrink ->
            pipeMinAndMax (Elm.Syntax.Expression.FunctionOrValue [ "Element" ] "shrink")


renderPaddingAttribute : Spec.Element.Layout.Padding.Padding -> Elm.Syntax.Expression.Expression
renderPaddingAttribute padding =
    let
        recordSetters =
            let
                { bottom, left, right, top } =
                    Spec.Element.Layout.Padding.deriveEachPadding padding

                toRecordSetter ( key, val ) =
                    ( node <| key
                    , node <| Elm.Syntax.Expression.Integer val
                    )
                        |> node
            in
            [ ( "bottom", bottom ), ( "left", left ), ( "right", right ), ( "top", top ) ]
                |> List.map toRecordSetter
    in
    renderElementFunc "paddingEach" <| Elm.Syntax.Expression.RecordExpr recordSetters


renderSpacingAttribute : Maybe Int -> Elm.Syntax.Expression.Expression
renderSpacingAttribute spacing =
    let
        spacingVal =
            Maybe.withDefault 0 spacing
    in
    renderElementFunc "spacing" (Elm.Syntax.Expression.Integer spacingVal)


renderAlignmentAttributes : Spec.Element.Layout.Alignment -> List Elm.Syntax.Expression.Expression
renderAlignmentAttributes alignment =
    let
        alignmentX =
            case alignment.x of
                Just Spec.Element.Layout.Left ->
                    Just "alignLeft"

                Just Spec.Element.Layout.CenterX ->
                    Just "centerX"

                Just Spec.Element.Layout.Right ->
                    Just "alignRight"

                Nothing ->
                    Nothing

        alignmentY =
            case alignment.y of
                Just Spec.Element.Layout.Top ->
                    Just "alignTop"

                Just Spec.Element.Layout.CenterY ->
                    Just "centerY"

                Just Spec.Element.Layout.Bottom ->
                    Just "alignBottom"

                Nothing ->
                    Nothing

        alignmentToExpression strAlignment =
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] strAlignment ]
    in
    [ alignmentX, alignmentY ]
        |> List.filterMap identity
        |> List.map alignmentToExpression



--


renderStyleAttributes : Model.Model.UserModel -> ( Spec.Element.Id.Id, Spec.Element.Style.Style ) -> Attributes
renderStyleAttributes { designSystem } ( id, styleDefs ) =
    let
        otherStyles =
            renderOverridesAttributes
                designSystem
                styleDefs

        -- typography =
        --     renderTypoAttributes designSystem styleDefs.typography
    in
    otherStyles



-- appendAttributes typography otherStyles


{-| @@TODO: Think about shadows and style composition
-}
renderOverridesAttributes :
    DesignSystem.Model
    -> Spec.Element.Style.Style
    -> Attributes
renderOverridesAttributes designSystem thisStyle =
    let
        -- DECORATION
        background =
            case thisStyle.background of
                Just (Ui.ColorPicker.Gradient.SolidBackground { selection }) ->
                    maybeRenderColor designSystem.colorEditor selection
                        |> Maybe.map (renderElementBackgroundFunc "color")
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []

                Just (Ui.ColorPicker.Gradient.GradientBackground gradient) ->
                    -- @TODO: gradient rendering
                    -- Ui.ColorPicker.Gradient.gradientToAttributes
                    --     gradient.gradient
                    --     designSystem.colorEditor
                    []

                Nothing ->
                    []

        shadow =
            renderSelectedShadowAttributes
                designSystem.colorEditor
                designSystem.shadows
                thisStyle.shadow

        borderRadiusRecordExpression : List Elm.Syntax.Expression.Expression
        borderRadiusRecordExpression =
            case thisStyle.roundedBorders of
                Nothing ->
                    []

                Just roundedBorders ->
                    let
                        { topLeft, bottomLeft, topRight, bottomRight } =
                            Spec.Element.Style.deriveCornerDimensions roundedBorders

                        setters : List Elm.Syntax.Expression.RecordSetter
                        setters =
                            [ ( "bottomRight"
                              , Elm.Syntax.Expression.Integer bottomRight
                              )
                            , ( "topRight"
                              , Elm.Syntax.Expression.Integer topRight
                              )
                            , ( "topLeft"
                              , Elm.Syntax.Expression.Integer topLeft
                              )
                            , ( "bottomLeft"
                              , Elm.Syntax.Expression.Integer bottomLeft
                              )
                            ]
                                |> List.map (Tuple.mapSecond node >> Tuple.mapFirst node)
                    in
                    setters
                        |> List.map node
                        |> Elm.Syntax.Expression.RecordExpr
                        |> renderElementBorderFunc "roundEach"
                        |> List.singleton

        renderedBorder =
            case thisStyle.borderSettings of
                Just b ->
                    renderBorderAttributes designSystem b

                Nothing ->
                    []

        decoration : List Elm.Syntax.Expression.Expression
        decoration =
            borderRadiusRecordExpression
                ++ background
                ++ renderedBorder
    in
    Attributes decoration []
        |> appendAttributes shadow



---- TYPO ----


renderTypoStyles : DesignSystem.Typography.Typo -> List Elm.Syntax.Expression.Expression
renderTypoStyles styles =
    let
        {- example: "Element.Font.size 13" -}
        size : Elm.Syntax.Expression.Expression
        size =
            let
                sizeVal =
                    styles.size |> Maybe.withDefault DesignSystem.Typography.defaultSize

                sizeApplication =
                    renderElementFontFunc "size" <| Elm.Syntax.Expression.Integer sizeVal
            in
            sizeApplication

        letterSpacing : Elm.Syntax.Expression.Expression
        letterSpacing =
            let
                letterSpacingVal =
                    styles.letterSpacing |> Maybe.withDefault DesignSystem.Typography.defaultLetterSpacing

                letterSpacingApplication =
                    renderElementFontFunc "letterSpacing" <| Elm.Syntax.Expression.Integer letterSpacingVal
            in
            letterSpacingApplication
    in
    [ size, letterSpacing ]



---- COLOR ----


maybeRenderColor : DesignSystem.Color.Model.Model -> DesignSystem.Color.Selection.Selection -> Maybe Elm.Syntax.Expression.Expression
maybeRenderColor model selection =
    case selection of
        DesignSystem.Color.Selection.FromSystem ref ->
            maybeRenderedDesignSystemColor model ref

        DesignSystem.Color.Selection.Standalone colorValue ->
            renderColor colorValue |> Just


renderColor : Color.Color -> Elm.Syntax.Expression.Expression
renderColor colorValue =
    let
        { red, green, blue, alpha } =
            Color.toRgba colorValue

        colorExpression : Elm.Syntax.Expression.Expression
        colorExpression =
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] "rgba"
                , node <| Elm.Syntax.Expression.Floatable red
                , node <| Elm.Syntax.Expression.Floatable green
                , node <| Elm.Syntax.Expression.Floatable blue
                , node <| Elm.Syntax.Expression.Floatable alpha
                ]
    in
    colorExpression |> renderParenthesize



---- SHADOW ----


renderSelectedShadowAttributes : DesignSystem.Color.Model.Model -> DesignSystem.Shadow.Model -> Spec.Element.Style.ShadowSelection -> Attributes
renderSelectedShadowAttributes colorModel shadowModel selection =
    case selection of
        Spec.Element.Style.ShadowFromDesignSystem index ->
            Attributes [] (renderedDesignSystemShadowAttributes shadowModel index)

        Spec.Element.Style.CustomShadow shadow ->
            Attributes (renderShadowAttributes colorModel shadow) []

        Spec.Element.Style.NoShadow ->
            Attributes [] []


renderShadowAttributes : DesignSystem.Color.Model.Model -> DesignSystem.Shadow.Shadow -> List Elm.Syntax.Expression.Expression
renderShadowAttributes colorModel shadow =
    let
        function =
            case shadow.type_ of
                DesignSystem.Shadow.OuterShadow ->
                    renderElementBorderFunc "shadow"

                DesignSystem.Shadow.InnerShadow ->
                    renderElementBorderFunc "innerShadow"

        maybeColor =
            maybeRenderColor
                colorModel
                shadow.color.selection

        config color =
            [ ( node "offset", renderOffset shadow.offset )
            , ( node "size", renderSize shadow.size )
            , ( node "blur", renderBlur shadow.blur )
            , ( node "color", color )
            ]
                |> List.map (Tuple.mapSecond node >> node)
                |> Elm.Syntax.Expression.RecordExpr

        renderOffset ( maybeX, maybeY ) =
            let
                offsetList =
                    [ Maybe.withDefault DesignSystem.Shadow.defaultOffsetX maybeX |> toFloat
                    , Maybe.withDefault DesignSystem.Shadow.defaultOffsetY maybeY |> toFloat
                    ]
                        |> List.map (Elm.Syntax.Expression.Floatable >> node)
            in
            Elm.Syntax.Expression.TupledExpression offsetList

        renderSize maybeSize =
            Maybe.withDefault DesignSystem.Shadow.defaultSize maybeSize
                |> toFloat
                |> Elm.Syntax.Expression.Floatable

        renderBlur maybeBlur =
            Maybe.withDefault DesignSystem.Shadow.defaultBlur maybeBlur
                |> toFloat
                |> Elm.Syntax.Expression.Floatable
    in
    case maybeColor of
        Just color_ ->
            [ function (config color_) ]

        Nothing ->
            []



---- BORDERS ----


renderBorderAttributes :
    DesignSystem.Model
    -> Spec.Element.Style.BorderSettings
    -> List Elm.Syntax.Expression.Expression
renderBorderAttributes designSystem { dimensions, color } =
    let
        { bottom, top, left, right } =
            dimensions
                |> Spec.Element.Style.Edges.deriveEdgeDimensions

        colorExpression : List Elm.Syntax.Expression.Expression
        colorExpression =
            maybeRenderColor designSystem.colorEditor color.selection
                |> Maybe.map (renderElementBorderFunc "color")
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        widths : Elm.Syntax.Expression.Expression
        widths =
            Elm.Syntax.Expression.Application
                [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element", "Border" ] "widthEach"
                , node borderRecordExpression
                ]

        borderRecordExpression : Elm.Syntax.Expression.Expression
        borderRecordExpression =
            let
                setters : List Elm.Syntax.Expression.RecordSetter
                setters =
                    [ ( "bottom"
                      , Elm.Syntax.Expression.Integer bottom
                      )
                    , ( "top"
                      , Elm.Syntax.Expression.Integer top
                      )
                    , ( "left"
                      , Elm.Syntax.Expression.Integer left
                      )
                    , ( "right"
                      , Elm.Syntax.Expression.Integer right
                      )
                    ]
                        |> List.map (Tuple.mapSecond node >> Tuple.mapFirst node)
            in
            setters
                |> List.map node
                |> Elm.Syntax.Expression.RecordExpr
    in
    widths :: colorExpression


dummyRange : Elm.Syntax.Range.Range
dummyRange =
    Elm.Syntax.Range.Range
        (Elm.Syntax.Range.Location 0 0)
        (Elm.Syntax.Range.Location 0 0)


node : a -> Elm.Syntax.Node.Node a
node =
    Elm.Syntax.Node.Node
        dummyRange


renderedIdentity : Elm.Syntax.Expression.Expression
renderedIdentity =
    Elm.Syntax.Expression.Application
        [ node <| Elm.Syntax.Expression.FunctionOrValue [] "identity" ]


renderLeftPipe : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderLeftPipe expr =
    Elm.Syntax.Expression.Application
        [ node <| Elm.Syntax.Expression.FunctionOrValue [] "<|"
        , node <| expr
        ]


renderAppendLists : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderAppendLists expr1 expr2 =
    Elm.Syntax.Expression.Application
        [ node <| renderParenthesize expr1
        , node <| Elm.Syntax.Expression.FunctionOrValue [] "++"
        , node <| renderParenthesize expr2
        ]


renderParenthesize : Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderParenthesize =
    Elm.Syntax.Expression.ParenthesizedExpression << node


renderElementFunc : String -> Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderElementFunc str expr =
    Elm.Syntax.Expression.Application
        [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element" ] str
        , node <| expr
        ]


renderElementBorderFunc : String -> Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderElementBorderFunc str expr =
    Elm.Syntax.Expression.Application
        [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element", "Border" ] str
        , node <| expr
        ]


renderElementFontFunc : String -> Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderElementFontFunc str expr =
    Elm.Syntax.Expression.Application
        [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element", "Font" ] str
        , node <| expr
        ]


renderElementBackgroundFunc : String -> Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression
renderElementBackgroundFunc str expr =
    Elm.Syntax.Expression.Application
        [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Element", "Background" ] str
        , node <| expr
        ]
