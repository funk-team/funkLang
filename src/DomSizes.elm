module DomSizes exposing
    ( Model
    , Msg
    , RefinedData
    , Status
    , decodeModel
    , encodeModel
    , get
    , getAll
    , removeManyRetrievedRects
    , toMaybe
    , update
    )

{-| Read the size of elements from the dom
-}

import Browser.Dom
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra
import Rectangle
import Spec.Model
import Task


toMaybe : Model -> Maybe Spec.Model.Rectangles
toMaybe model =
    case model of
        Done r ->
            Just r

        _ ->
            Nothing


init : Model
init =
    Retrieving initData


type alias Data sh r =
    { sheet : sh, drawn : r }


initData : WipData
initData =
    Data Asked Spec.Element.Id.emptyDict


type alias WipData =
    Data (Status Browser.Dom.Element) LoadingElements


type alias LoadingElements =
    Spec.Element.Id.Dict (Status Browser.Dom.Element)


get : Spec.Element.Id.Id -> Model -> Maybe Rectangle.Rectangle
get id model =
    case model of
        Done statuses ->
            Spec.Element.Id.getFromDict id statuses

        _ ->
            Nothing


removeManyRetrievedRects : Spec.Model.Selection -> Model -> Model
removeManyRetrievedRects selection model =
    case model of
        Done statuses ->
            Done <| Spec.Element.Id.removeManyFromDict selection statuses

        _ ->
            model


update : Msg -> Model -> Model
update msg model =
    let
        update_ : WipData -> Model
        update_ statuses =
            case msg of
                ElementSizeRetrieved id el ->
                    { statuses | drawn = Spec.Element.Id.insertIntoDict id (Retrieved el) statuses.drawn }
                        |> transformIfDone

                SheetSizeRetrieved sh ->
                    { statuses | sheet = Retrieved sh }
                        |> transformIfDone

                GotProblem err ->
                    Error err
    in
    case model of
        Retrieving statuses ->
            update_ statuses

        -- ignore final situations
        Error _ ->
            model

        Done _ ->
            model


{-| Move the coordinate origin of rect 2 to the top left corner of rect 1
-}
makeRelative : Browser.Dom.Element -> Browser.Dom.Element -> Rectangle.Rectangle
makeRelative sheet { element } =
    let
        relative =
            Rectangle.fromDimensionsFloat element
                |> Rectangle.moveBy
                    -sheet.element.x
                    -sheet.element.y
    in
    relative


{-| Get rectangles positions based on sheet and element location
-}
refine : CompleteData -> RefinedData
refine { drawn, sheet } =
    drawn
        |> Spec.Element.Id.dictToList
        |> List.map (Tuple.mapSecond (makeRelative sheet))
        |> Spec.Element.Id.dictFromList


transformIfDone : WipData -> Model
transformIfDone statuses =
    let
        { sheet, drawn } =
            statuses

        -- extract all rectangle dom dimensions if
        -- the status is `Retrieved`
        distilledRects =
            drawn
                |> Spec.Element.Id.dictToList
                |> List.map statusToMaybe
                |> Maybe.Extra.combine
    in
    case ( sheet, distilledRects ) of
        ( Retrieved sh, Just resultingRects ) ->
            refine { sheet = sh, drawn = Spec.Element.Id.dictFromList resultingRects }
                |> Done

        _ ->
            Retrieving statuses


statusToMaybe ( id, status ) =
    case status of
        Retrieved status_ ->
            Just ( id, status_ )

        _ ->
            Nothing


getAll : List Spec.Element.Id.Id -> ( Model, Cmd Msg )
getAll ids =
    let
        cmds =
            ids
                |> List.filter ((==) Spec.Element.Id.rootId >> not)
                |> List.map
                    (\id ->
                        let
                            task =
                                Browser.Dom.getElement (Spec.Element.Id.toHtmlIdRaw id)
                        in
                        Task.attempt (onElRetrieved id) task
                    )
                |> Cmd.batch

        sheetCmd =
            Spec.Element.Id.rootId
                |> Spec.Element.Id.toHtmlIdRaw
                |> Browser.Dom.getElement
                |> Task.attempt onSheetRetrieved
    in
    ( init, Cmd.batch [ sheetCmd, cmds ] )


onElRetrieved :
    Spec.Element.Id.Id
    -> Result Browser.Dom.Error Browser.Dom.Element
    -> Msg
onElRetrieved id res =
    case res of
        Err err ->
            GotProblem err

        Ok el ->
            ElementSizeRetrieved id el


onSheetRetrieved :
    Result Browser.Dom.Error Browser.Dom.Element
    -> Msg
onSheetRetrieved res =
    case res of
        Err err ->
            GotProblem err

        Ok sh ->
            SheetSizeRetrieved sh


type alias RectangleStatuses =
    Spec.Element.Id.Dict (Status Browser.Dom.Element)


type alias SheetStatus =
    Status Browser.Dom.Element


type Status sth
    = Asked
    | Problem
    | Retrieved sth


type Msg
    = ElementSizeRetrieved Spec.Element.Id.Id Browser.Dom.Element
    | SheetSizeRetrieved Browser.Dom.Element
    | GotProblem Browser.Dom.Error


type alias CompleteData =
    { sheet : Browser.Dom.Element, drawn : Spec.Element.Id.Dict Browser.Dom.Element }


type alias RefinedData =
    Spec.Model.Rectangles


type Model
    = Retrieving WipData
    | Error Browser.Dom.Error
    | Done RefinedData


encodeModel _ =
    Encode.null


decodeModel =
    Decode.succeed init
