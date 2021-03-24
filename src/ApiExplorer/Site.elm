module ApiExplorer.Site exposing (..)

import ApiExplorer.Request
import Element
import Element.Background
import Element.Border
import Element.Input
import Html
import Html.Attributes
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import RemoteData
import Ui.Style


distillContent : Int -> Html.Parser.Node -> ( Path, DistilledContent ) -> ( Path, DistilledContent )
distillContent index next ( path, accum ) =
    let
        result =
            case next of
                Html.Parser.Element "title" _ ((Html.Parser.Text title) :: _) ->
                    { accum | title = Just title }

                Html.Parser.Element "img" attribs _ ->
                    { accum | bytes = accum.bytes ++ extractImageByte (index :: path) attribs }

                -- script content is not necessary
                Html.Parser.Element "script" _ _ ->
                    accum

                Html.Parser.Element tag _ children ->
                    case List.member tag richTextElements of
                        True ->
                            case children of
                                [ Html.Parser.Text text ] ->
                                    { accum | bytes = accum.bytes ++ [ TaggedContentByte (List.reverse (index :: path)) (Text text) ] }

                                _ ->
                                    { accum | bytes = accum.bytes ++ [ TaggedContentByte (List.reverse (index :: path)) (RichText children) ] }

                        False ->
                            let
                                ( _, subResult ) =
                                    List.Extra.indexedFoldl
                                        distillContent
                                        ( index :: path, accum )
                                        children
                            in
                            subResult

                -- empty text should not be available
                Html.Parser.Text t ->
                    case String.trim t of
                        "" ->
                            accum

                        _ ->
                            { accum
                                | bytes =
                                    accum.bytes
                                        ++ [ TaggedContentByte (List.reverse (index :: path)) (Text t) ]
                            }

                Html.Parser.Comment _ ->
                    accum
    in
    ( path, result )


type Msg
    = ByteSelected TaggedContentByte
    | MakeList (List TaggedContentByte) ListPath
    | RemoveList Int


type alias Request =
    RemoteData.WebData ApiExplorer.Request.HtmlResponseData


encodeRequest : Request -> Encode.Value
encodeRequest =
    RemoteData.map ApiExplorer.Request.encodeHtmlResponseData
        >> RemoteData.withDefault Encode.null


decodeRequest : Decode.Decoder Request
decodeRequest =
    Decode.oneOf
        [ ApiExplorer.Request.decodeHtmlResponseData
            |> Decode.map RemoteData.Success
        , Decode.succeed RemoteData.NotAsked
        ]



-- [generator-start]


type HtmlContentByte
    = Image ImageData
    | RichText ApiExplorer.Request.HtmlResponseData
    | Text String


type alias ImageData =
    { src : String
    , alt : Maybe String
    }


type alias DistilledContent =
    { title : Maybe String
    , bytes : List TaggedContentByte
    }


type alias Path =
    List Int


type alias ListPath =
    ( Path, Path )


type alias TaggedContentByte =
    { path : Path
    , value : HtmlContentByte
    }


type alias Model =
    { request : Request
    , selection : List TaggedContentByte
    , lists : List ListPath
    , url : String
    }



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeDistilledContent =
    Decode.map2
        DistilledContent
        (Decode.field "title" (Decode.maybe Decode.string))
        (Decode.field "bytes" (Decode.list decodeTaggedContentByte))


decodeHtmlContentByte =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeHtmlContentByteHelp


decodeHtmlContentByteHelp constructor =
    case constructor of
        "Image" ->
            Decode.map
                Image
                (Decode.field "A1" decodeImageData)

        "RichText" ->
            Decode.map
                RichText
                (Decode.field "A1" ApiExplorer.Request.decodeHtmlResponseData)

        "Text" ->
            Decode.map
                Text
                (Decode.field "A1" Decode.string)

        other ->
            Decode.fail <| "Unknown constructor for type HtmlContentByte: " ++ other


decodeImageData =
    Decode.map2
        ImageData
        (Decode.field "src" Decode.string)
        (Decode.field "alt" (Decode.maybe Decode.string))


decodeListPath =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodePath)
        (Decode.field "A2" decodePath)


decodeModel =
    Decode.map4
        Model
        (Decode.field "request" decodeRequest)
        (Decode.field "selection" (Decode.list decodeTaggedContentByte))
        (Decode.field "lists" (Decode.list decodeListPath))
        (Decode.field "url" Decode.string)


decodePath =
    Decode.list Decode.int


decodeTaggedContentByte =
    Decode.map2
        TaggedContentByte
        (Decode.field "path" decodePath)
        (Decode.field "value" decodeHtmlContentByte)


encodeDistilledContent a =
    Encode.object
        [ ( "title", encodeMaybeString a.title )
        , ( "bytes", Encode.list encodeTaggedContentByte a.bytes )
        ]


encodeHtmlContentByte a =
    case a of
        Image a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Image" )
                , ( "A1", encodeImageData a1 )
                ]

        RichText a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "RichText" )
                , ( "A1", ApiExplorer.Request.encodeHtmlResponseData a1 )
                ]

        Text a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Text" )
                , ( "A1", Encode.string a1 )
                ]


encodeImageData a =
    Encode.object
        [ ( "src", Encode.string a.src )
        , ( "alt", encodeMaybeString a.alt )
        ]


encodeListPath ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodePath a1 )
        , ( "A2", encodePath a2 )
        ]


encodeMaybeString a =
    case a of
        Just b ->
            Encode.string b

        Nothing ->
            Encode.null


encodeModel a =
    Encode.object
        [ ( "request", encodeRequest a.request )
        , ( "selection", Encode.list encodeTaggedContentByte a.selection )
        , ( "lists", Encode.list encodeListPath a.lists )
        , ( "url", Encode.string a.url )
        ]


encodePath a =
    Encode.list Encode.int a


encodeTaggedContentByte a =
    Encode.object
        [ ( "path", encodePath a.path )
        , ( "value", encodeHtmlContentByte a.value )
        ]



-- [generator-end]


view : Model -> Element.Element Msg
view model =
    case model.request of
        RemoteData.Success html ->
            Element.column [ Element.spacing 100 ]
                [ Element.text model.url
                , selectionView model html
                , contentView model html
                , List.head html |> Maybe.map Html.Parser.nodeToString |> Maybe.withDefault "" |> Element.text
                ]

        _ ->
            Element.text "No content available right now"


previewList : ContentList -> Element.Element msg
previewList l =
    case l of
        ImageList images ->
            previewImageList images

        RichTextList texts ->
            Element.row
                []
                [ Element.text (String.fromInt (List.length texts))
                , Element.text "Formatted Texts"
                ]

        TextList texts ->
            Element.row
                []
                [ Element.text (String.fromInt (List.length texts))
                , Element.text "Simple Texts"
                ]


previewImageList images =
    let
        previewCount =
            3

        gallerySize =
            List.length images

        previews =
            List.take previewCount images
                |> List.map (\{ src, alt } -> Element.image [ Element.width (Element.px 100) ] { src = src, description = alt |> Maybe.withDefault "image preview" })

        suffix =
            if gallerySize > 3 then
                Element.text ("And " ++ String.fromInt (gallerySize - 3) ++ " more.")

            else
                Element.none
    in
    Element.row [ Element.spacing 5 ] (previews ++ [ suffix ])


selectionView : Model -> ApiExplorer.Request.HtmlResponseData -> Element.Element Msg
selectionView model html =
    let
        viewSelection sel =
            viewByte model sel
                |> Element.map ByteSelected

        sortedSelections =
            model.selection
                |> List.sortBy (.path >> List.map String.fromInt >> String.join " ")

        viewList : Int -> ListPath -> Element.Element Msg
        viewList index list =
            let
                removeListBUtton =
                    Element.Input.button
                        []
                        { onPress = Just (RemoveList index)
                        , label = Element.text "[remove]"
                        }

                listPreview =
                    case getList list model of
                        Nothing ->
                            Element.none

                        Just l ->
                            previewList l
            in
            Element.column
                []
                [ listPreview
                , removeListBUtton
                ]

        {- Byte selections -}
        allSelections : List (Element.Element Msg)
        allSelections =
            case sortedSelections |> List.map viewSelection of
                [] ->
                    [ Element.text "No bytes selected" ]

                v ->
                    v

        {- List Selection -}
        allLists : List (Element.Element Msg)
        allLists =
            model.lists
                |> List.indexedMap viewList

        bits =
            Element.column
                [ Element.spacing 10, Element.padding 10, Element.Background.color Ui.Style.white ]
                (Element.text "Selected Bits" :: allSelections)

        lists =
            Element.column
                [ Element.spacing 10, Element.padding 10, Element.Background.color Ui.Style.white ]
                (Element.text "Selected Lists" :: allLists)
    in
    Element.column [ Element.spacing 10 ]
        [ bits
        , lists
        , similaritiesSuggestions model sortedSelections
        ]



-- [generator-start]


type ContentList
    = ImageList (List ImageData)
    | RichTextList (List ApiExplorer.Request.HtmlResponseData)
    | TextList (List String)



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeContentList =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeContentListHelp


decodeContentListHelp constructor =
    case constructor of
        "ImageList" ->
            Decode.map
                ImageList
                (Decode.field "A1" (Decode.list decodeImageData))

        "RichTextList" ->
            Decode.map
                RichTextList
                (Decode.field "A1" (Decode.list ApiExplorer.Request.decodeHtmlResponseData))

        "TextList" ->
            Decode.map
                TextList
                (Decode.field "A1" (Decode.list Decode.string))

        other ->
            Decode.fail <| "Unknown constructor for type ContentList: " ++ other


encodeContentList a =
    case a of
        ImageList a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ImageList" )
                , ( "A1", Encode.list encodeImageData a1 )
                ]

        RichTextList a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "RichTextList" )
                , ( "A1", Encode.list ApiExplorer.Request.encodeHtmlResponseData a1 )
                ]

        TextList a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "TextList" )
                , ( "A1", Encode.list Encode.string a1 )
                ]



-- [generator-end]


getList : ListPath -> Model -> Maybe ContentList
getList ( before, after ) model =
    case model.request of
        RemoteData.Success html ->
            let
                getNext : Path -> Html.Parser.Node -> Maybe Html.Parser.Node
                getNext path node =
                    case ( path, node ) of
                        ( next :: tail, Html.Parser.Element _ _ children ) ->
                            case List.Extra.getAt next children of
                                Nothing ->
                                    Nothing

                                Just nextNode ->
                                    getNext tail nextNode

                        ( next :: tail, _ ) ->
                            Nothing

                        ( [], _ ) ->
                            Just node

                listRoot =
                    Html.Parser.Element "all" [] html
                        |> getNext before

                list =
                    case listRoot of
                        Just (Html.Parser.Element _ _ children) ->
                            List.filterMap (getNext after) children
                                |> List.filterMap
                                    (\node ->
                                        case node of
                                            Html.Parser.Element "img" attribs _ ->
                                                extractImage attribs

                                            _ ->
                                                Nothing
                                    )
                                |> ImageList
                                |> Just

                        _ ->
                            Nothing
            in
            list

        _ ->
            Nothing


similaritiesSuggestions : Model -> List TaggedContentByte -> Element.Element Msg
similaritiesSuggestions model sortedSelections =
    let
        initListCandidates : ListCandidates
        initListCandidates =
            { prev = Nothing, cluster = Nothing, result = [] }

        similarities =
            List.foldl findCandidate initListCandidates sortedSelections
                |> flush
                |> viewSimilarities

        flush : ListCandidates -> List ListCandidate
        flush { cluster, result } =
            case cluster of
                Just validCluster ->
                    validCluster :: result

                Nothing ->
                    result

        viewListCandidate : ListCandidate -> Element.Element Msg
        viewListCandidate candidate =
            case List.head candidate.members of
                Nothing ->
                    Element.none

                Just firstByte ->
                    let
                        listPath =
                            ( List.take candidate.position firstByte.path
                            , List.drop (candidate.position + 1) firstByte.path
                            )

                        extrapolation =
                            getList listPath model

                        acceptButton =
                            Element.Input.button [] { onPress = Just msg, label = Element.text "Accept" }

                        msg =
                            MakeList candidate.members listPath
                    in
                    Element.column []
                        [ Element.text (String.fromInt candidate.position)
                        , acceptButton
                        , Element.row [ Element.spacing 5 ] (List.map (preview model) candidate.members)
                        , Element.text <|
                            "Found "
                                ++ (case extrapolation of
                                        Just (ImageList images) ->
                                            List.length images
                                                |> String.fromInt

                                        Just (RichTextList texts) ->
                                            List.length texts
                                                |> String.fromInt

                                        Just (TextList texts) ->
                                            List.length texts
                                                |> String.fromInt

                                        Nothing ->
                                            "0"
                                   )
                        ]

        viewSimilarities : List ListCandidate -> Element.Element Msg
        viewSimilarities candidates =
            case candidates of
                [] ->
                    let
                        viewCs =
                            Element.text "No lists detected"
                                |> Element.el [ Element.Background.color Ui.Style.white ]
                    in
                    Element.column
                        [ Element.Background.color Ui.Style.white ]
                        [ Element.text "Auto Lists", viewCs ]

                _ ->
                    let
                        viewCs =
                            candidates
                                |> List.map viewListCandidate
                                |> Element.column [ Element.spacing 10 ]
                    in
                    Element.column
                        [ Element.Background.color Ui.Style.white ]
                        [ Element.text "Lists", viewCs ]
    in
    similarities



{- If paths differ at one position only we can assume they can be grouped
   caveat: for now, lists with multiple values per entry are not supported
-}


findSingularDifferentPosition : Path -> Path -> Maybe Int
findSingularDifferentPosition p1 p2 =
    if List.length p1 == List.length p2 then
        let
            diffingPositions =
                List.map2 Tuple.pair p1 p2
                    |> List.indexedMap
                        (\idx ( node1, node2 ) ->
                            if node1 == node2 then
                                Nothing

                            else
                                Just idx
                        )
                    |> List.filterMap identity
        in
        case diffingPositions of
            [ onePos ] ->
                Just onePos

            _ ->
                Nothing

    else
        Nothing


{-|

  - [0, 1, 2]

  - [1,2,3,4,5]

  - [1,2,4,4,5]

  - [1,2,5,4,5]

  - [1,2,6,4,5]

  - [2, 1, 2]
    0 1 2

  - [1,2,5,4,5]

  - [1,2,6,4,5]
    --> [2, 4] -- more difficult to make a list from it
    2 -> list iteration

-}
findCandidate : TaggedContentByte -> ListCandidates -> ListCandidates
findCandidate next current =
    (\summary -> { summary | prev = Just next }) <|
        case current.prev of
            Nothing ->
                current

            Just prev ->
                case findSingularDifferentPosition next.path prev.path of
                    -- in case the current differs from the previous, flush the currently observed cluster
                    Nothing ->
                        case current.cluster of
                            Nothing ->
                                { current | cluster = Nothing }

                            Just validCluster ->
                                { current | cluster = Nothing, result = validCluster :: current.result }

                    Just pos ->
                        case current.cluster of
                            -- two initial candidates work well together, initialize the currently observed cluster
                            Nothing ->
                                { current
                                    | cluster =
                                        Just
                                            { position = pos
                                            , members = [ next, prev ]
                                            }
                                }

                            Just validCluster ->
                                if validCluster.position == pos then
                                    { current
                                        | cluster = Just { validCluster | members = next :: validCluster.members }
                                    }

                                else
                                    { current | cluster = Nothing, result = validCluster :: current.result }


type alias ListCandidates =
    { prev : Maybe TaggedContentByte
    , cluster : Maybe ListCandidate
    , result : List ListCandidate
    }


type alias ListCandidate =
    { members : List TaggedContentByte
    , position : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ByteSelected newSelectedByte ->
            let
                -- toggle if it was previously selected
                selection =
                    case List.Extra.findIndex (\someSelection -> someSelection.path == newSelectedByte.path) model.selection of
                        Just index ->
                            List.Extra.removeAt index model.selection

                        Nothing ->
                            newSelectedByte :: model.selection
            in
            ( { model | selection = selection }
            , Cmd.none
            )

        MakeList selectedMembers path ->
            ( { model
                | selection =
                    List.filter
                        (\selectedByte -> List.member selectedByte selectedMembers |> not)
                        model.selection
                , lists = path :: model.lists
              }
            , Cmd.none
            )

        RemoveList index ->
            ( { model
                | lists = List.Extra.removeAt index model.lists
              }
            , Cmd.none
            )


contentView :
    Model
    -> List Html.Parser.Node
    -> Element.Element Msg
contentView model html =
    html
        |> List.Extra.indexedFoldl distillContent initDistillation
        |> Tuple.second
        |> viewDistilledContent model
        |> Element.map ByteSelected


viewDistilledContent : Model -> DistilledContent -> Element.Element TaggedContentByte
viewDistilledContent model { title, bytes } =
    let
        titleFound =
            case title of
                Just t ->
                    Element.text <| "Title: " ++ t

                Nothing ->
                    Element.text "Title not detected"

        bytesFound =
            List.map (viewByte model) bytes
    in
    Element.column
        [ Element.width Element.fill, Element.spacing 10 ]
        (Element.text "Distilled content" :: titleFound :: bytesFound)


preview model { value } =
    case value of
        Text text ->
            Element.el
                [ Ui.Style.style "max-width" "60ch", Ui.Style.style "overflow" "hidden", Ui.Style.style "text-overflow" "ellipsis" ]
                (Element.text text)

        RichText els ->
            Element.html
                (Html.Parser.Util.toVirtualDom els
                    |> Html.p [ Html.Attributes.style "max-width" "60ch", Html.Attributes.style "overflow" "hidden", Html.Attributes.style "text-overflow" "ellipsis" ]
                )
                |> Element.el []

        Image { src, alt } ->
            let
                imgTitle =
                    Maybe.withDefault "" alt

                imgSrc =
                    case String.startsWith "/" src of
                        True ->
                            model.url ++ src

                        False ->
                            src
            in
            Element.column []
                [ Element.text imgTitle
                , Element.image [ Element.width (Element.px 100) ] { src = imgSrc, description = imgTitle }
                ]


viewByte : Model -> TaggedContentByte -> Element.Element TaggedContentByte
viewByte model ({ path, value } as taggedByte) =
    let
        byteView =
            Element.column []
                [ preview model taggedByte
                ]

        checkbox =
            let
                checked =
                    List.member taggedByte model.selection
            in
            Element.Input.checkbox
                [ Element.padding 10
                , Element.Border.width 1
                , Element.Border.rounded 5
                , Element.width Element.shrink
                , Element.Border.color
                    (if checked then
                        Ui.Style.highlightColorSolid

                     else
                        Ui.Style.slightAccent
                    )
                ]
                { onChange = \_ -> taggedByte
                , icon = Element.Input.defaultCheckbox
                , label = Element.Input.labelRight [] byteView
                , checked = checked
                }
    in
    checkbox


initDistillation =
    ( [], { title = Nothing, bytes = [] } )


extractImageByte : Path -> List Html.Parser.Attribute -> List TaggedContentByte
extractImageByte path attribs =
    [ Maybe.map (TaggedContentByte (List.reverse path)) <| Maybe.map Image <| extractImage attribs ]
        |> List.filterMap identity


extractImage : List Html.Parser.Attribute -> Maybe ImageData
extractImage attribs =
    let
        src : Maybe String
        src =
            List.Extra.find
                (\( attrName, attrValue ) ->
                    case attrName of
                        "src" ->
                            True

                        _ ->
                            False
                )
                attribs
                |> Maybe.map Tuple.second

        alt : Maybe String
        alt =
            List.Extra.find
                (\( attrName, attrValue ) ->
                    case attrName of
                        "alt" ->
                            True

                        _ ->
                            False
                )
                attribs
                |> Maybe.map Tuple.second
    in
    case src of
        Just s ->
            Just (ImageData s alt)

        Nothing ->
            Nothing


richTextElements =
    [ "p", "h1", "h2", "h3", "h4", "h5", "h6", "span" ]
