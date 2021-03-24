module ResponsifyTestingEnvironment exposing (..)

import Canvas.Events exposing (AbsoluteRectangle(..))
import Canvas.Selection
import Canvas.Tool.Responsify exposing (previewPrediction)
import Canvas.Tool.Responsify.Engine exposing (runResponsifyEngine)
import Canvas.Tool.Responsify.Info exposing (encodeInfo)
import Canvas.Tool.Responsify.Model exposing (ResponsifyExport, decodeResponsifyExport)
import Canvas.View exposing (renderShared)
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font as Font
import Interface.Scope
import Json.Decode as Decode
import Json.Diff exposing (cheapDiff)
import Json.Encode as Encode
import Json.Patch.Invertible as InvPatch
import Json.Pointer as Pointer exposing (Pointer)
import List.Extra
import Model
import Model.Model
import Rectangle
import RemoteData
import RemoteData.Http
import ResponsifyTestingEnvironment.Model exposing (..)
import Spec.Element.Layout.Length as Length
import Spec.Element.Model exposing (EitherElement, EitherOuterGeometry(..), ScreenSize(..), encodeEitherElement)
import Ui.RadioRow
import Ui.Style


type Msg
    = GotTests TestsData
    | TestSelected String
    | InputOutputChanged InputOutputDisplay
    | NoOp


getTestsData : Cmd Msg
getTestsData =
    RemoteData.Http.get
        "/api/responsify-examples"
        GotTests
        decodeTestsData


decodeTestsData =
    Decode.map2 Tuple.pair
        (Decode.field "name" Decode.string)
        (Decode.field "content" decodeResponsifyExport)
        |> Decode.list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTests tests ->
            ( { model
                | tests = tests
                , activeTest =
                    case tests of
                        RemoteData.Success (( firstTestName, _ ) :: _) ->
                            Just firstTestName

                        _ ->
                            Nothing
              }
            , Cmd.none
            )

        TestSelected activeTest ->
            ( { model | activeTest = Just activeTest }, Cmd.none )

        InputOutputChanged inout ->
            ( { model | display = inout }, Cmd.none )


initCmd =
    getTestsData


view : Model.Model.Model -> Element.Element Msg
view mainModel =
    let
        model =
            mainModel.responsifyTests

        foundTest =
            case ( model.activeTest, model.tests ) of
                ( Just testName, RemoteData.Success tests ) ->
                    tests
                        |> List.Extra.find (\( name, _ ) -> name == testName)

                _ ->
                    Nothing
    in
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewSidebar mainModel
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (case foundTest of
                Nothing ->
                    Element.text "select a test from the left"

                Just test ->
                    viewTestVisual mainModel test
            )
        ]


processPatch : InvPatch.Patch -> List String
processPatch =
    List.concatMap processPatchOp


showPointer : Pointer -> String
showPointer p =
    Encode.encode 0 <| Pointer.encoder p


processPatchOp : InvPatch.Operation -> List String
processPatchOp op =
    case op of
        InvPatch.Add p v ->
            [ "ADD " ++ showPointer p, "  " ++ Encode.encode 0 v, " " ]

        InvPatch.Remove p v ->
            [ "REMOVE " ++ showPointer p, "  " ++ Encode.encode 0 v, " " ]

        InvPatch.Replace p v1 v2 ->
            [ "REPLACE " ++ showPointer p
            , "  " ++ Encode.encode 0 v1
            , "  " ++ Encode.encode 0 v2
            , " "
            ]

        InvPatch.Move p1 p2 ->
            [ "MOVE " ++ showPointer p1 ++ " -> " ++ showPointer p2, " " ]



-- Fix element to origin for rendering of overlay.


elementToOrigin : EitherElement -> EitherElement
elementToOrigin elem =
    let
        zeroScreen : ScreenSize -> ScreenSize
        zeroScreen ssize =
            case ssize of
                Preset dev _ ->
                    Preset dev { x = 0, y = 0 }

                Custom (AbsoluteRectangle rect) ->
                    Custom <| AbsoluteRectangle <| Rectangle.moveToOrigin rect
    in
    case elem.outerGeometry of
        AbsoluteElementGeometry dims ->
            { elem
                | outerGeometry =
                    AbsoluteElementGeometry
                        { dims
                            | x = Length.init 0
                            , y = Length.init 0
                        }
            }

        ScreenGeometry ssize ->
            { elem
                | outerGeometry =
                    ScreenGeometry (zeroScreen ssize)
            }

        _ ->
            elem


viewTestVisual : Model.Model.Model -> ( String, ResponsifyExport ) -> Element.Element Msg
viewTestVisual model ( _, { input, prediction } ) =
    let
        bg =
            Element.Background.color Ui.Style.slightAccent

        userModel =
            Model.latest model

        visualization : InputOutputDisplay -> EitherElement -> Element.Element Msg
        visualization display overlay =
            let
                visualizedOverlay =
                    overlay
                        |> previewPrediction (Model.latest model) mockScope
                        |> List.map (Element.mapAttribute (always NoOp))

                ( _, Canvas.Events.ElementRectangle parentRect ) =
                    input.parentDimensions

                dimensions =
                    Rectangle.render (Rectangle.moveToOrigin parentRect)

                children =
                    visualizeChildren display overlay
            in
            Element.el (dimensions ++ children ++ visualizedOverlay) Element.none

        visualizeChildren : InputOutputDisplay -> EitherElement -> List (Element.Attribute Msg)
        visualizeChildren inout overlay =
            case inout of
                DisplayInput ->
                    input.siblingsDimensions
                        |> List.map (\( id, Canvas.Events.ElementRectangle child ) -> Element.el (bg :: Rectangle.render child) Element.none)
                        |> List.map Element.inFront

                DisplayOutput ->
                    let
                        zeroedElement =
                            elementToOrigin input.element

                        context =
                            { model = model
                            , selectionItem = Canvas.Selection.fresh zeroedElement.shared.id
                            , scope = mockScope
                            , parent = Just zeroedElement
                            }
                    in
                    renderShared context (elementToOrigin overlay) []
                        |> List.map (Element.map (always NoOp) >> Element.behindContent)

        -- just an empty scope
        mockScope =
            Interface.Scope.empty

        expectedOutput =
            prediction

        ( maybeRule, rulesInfo, actualOutput ) =
            runResponsifyEngine userModel input

        patches =
            cheapDiff
                (encodeEitherElement actualOutput)
                (encodeEitherElement expectedOutput)
                |> processPatch

        rulesView =
            [ Element.row [ Font.bold ]
                [ Element.text "RULE SELECTED" ]
            , Element.column [ Font.family [ Font.monospace ], Font.size 12 ] <|
                case maybeRule of
                    Nothing ->
                        [ Element.text "<none>" ]

                    Just "fallback" ->
                        [ Element.text "FALLBACK (NO VALID RULE)" ]

                    Just rule ->
                        [ Element.text rule ]
            ]

        infoView =
            [ Element.row [ Font.bold ]
                [ Element.text "RULES INFO" ]
            , Element.paragraph [ Font.family [ Font.monospace ], Font.size 12 ]
                [ Element.text <| Encode.encode 0 <| encodeInfo rulesInfo ]
            ]

        patchView =
            if List.isEmpty patches then
                []

            else
                [ Element.row [ Font.bold ]
                    [ Element.text "ACTUAL ⇒ EXPECTED" ]
                , Element.column [ Font.family [ Font.monospace ], Font.size 12 ]
                    (patches
                        |> List.map (Element.row [] << List.singleton << Element.text)
                    )
                ]

        inputOutputButtons =
            { items = [ DisplayInput, DisplayOutput ]
            , toLabel =
                \choice ->
                    case choice of
                        DisplayInput ->
                            "INPUT"

                        DisplayOutput ->
                            "OUTPUT"
            , selected = (==) model.responsifyTests.display
            }
    in
    Element.column [ Element.padding 20, Element.spacing 40 ]
        [ Element.row [ Element.spacing 20 ]
            [ Element.column [ Element.spacing 10 ]
                [ Element.text "expected", visualization DisplayOutput expectedOutput ]
            , Element.column [ Element.spacing 10 ]
                [ Element.row [ Element.spacing 20 ]
                    [ Element.text "actual"
                    , Element.el
                        [ Font.size 8
                        , Element.alignTop
                        ]
                        (Ui.RadioRow.view inputOutputButtons)
                        |> Element.map InputOutputChanged
                    ]
                , visualization model.responsifyTests.display actualOutput
                ]
            ]
        , Element.column [ Element.spacing 20 ] <|
            rulesView
                ++ patchView
                ++ infoView
        ]


viewSidebar mainModel =
    let
        model =
            mainModel.responsifyTests
    in
    Element.column
        [ Element.width <| Element.px 256
        , Element.height <| identity <| Element.minimum 568 <| Element.fill
        , Element.Background.color Ui.Style.slightAccent
        ]
    <|
        case model.tests of
            RemoteData.Success tests ->
                List.map (viewTestInSidebar (Model.latest mainModel) model.activeTest) tests

            RemoteData.Failure f ->
                [ Element.text "Could not load tests" ]

            _ ->
                [ Element.text "loading" ]


viewTestInSidebar : Model.Model.UserModel -> Maybe String -> ( String, ResponsifyExport ) -> Element.Element Msg
viewTestInSidebar userModel activeTest ( testName, testContent ) =
    let
        ( rules, rulesInfo, actualOutput ) =
            runResponsifyEngine userModel testContent.input

        testStatus =
            actualOutput == testContent.prediction

        isSelected =
            Just testName == activeTest
    in
    Element.row
        [ Element.spacing 10
        , Element.width Element.fill
        , Element.height Element.shrink
        , Element.Background.color
            (if isSelected then
                Ui.Style.white

             else
                Ui.Style.transparent
            )
        , Element.Events.onClick (TestSelected testName)
        , Element.pointer
        , Element.padding 5
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.centerY
            , Element.clip
            ]
            (Element.text testName)
        , viewTestStatus testStatus
        ]


viewTestStatus status =
    let
        green =
            Element.rgb255 94 186 125

        red =
            Element.rgb 1 0 0
    in
    Element.el
        [ Element.alignRight
        , Element.width <| Element.px 20
        , Element.height <| Element.px 20
        , Element.Border.rounded 10
        , Element.centerY
        , Element.Background.color
            (if status then
                green

             else
                red
            )
        ]
        Element.none
