module ApiExplorer.Api exposing (..)

{-| A GUI for communication with backends.
Example: GET JSON to dict tree data structure with labels defined by the user
-}

import ApiExplorer.Api.UrlParser
import ApiExplorer.Curl
import ApiExplorer.Help exposing (greyBackgroundStyle, viewSection, viewValidCheck)
import ApiExplorer.Mock
import ApiExplorer.Model
import ApiExplorer.Msg
import ApiExplorer.Request
import Dict
import Dict.Any
import Dynamic.Data
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Html exposing (text)
import Http
import Interface.Data
import Interface.JsonTree
import Interface.JsonTree.Model
import Interface.Selection
import Interpolator
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Persistence
import RemoteData
import RemoteData.Http
import Ui.Boxicons
import Ui.Component
import Ui.Dropdown
import Ui.Style
import Ui.Table2


pathColumnWidth =
    Element.px 550


typeColumnWidth =
    Element.px 160


nameColumnWidth =
    Element.px 400


{-| A list of parameters to be injected into the API call
-}
type alias ApiParams =
    List ApiParam


type alias ApiParam =
    { keyPath : Interface.JsonTree.Model.KeyPath
    , variable : Maybe ParamVariable
    , refinedType : Interface.Selection.RefinedType
    , data : Dynamic.Data.Instance
    , fieldName : String
    , fieldKey : Int
    }


type alias ParamVariable =
    { key : Interface.Selection.InterpolationVariableKey
    , name : String
    , refinedType : Interface.Selection.RefinedType
    }


bakeBody : { body : String, params : ApiParams } -> Maybe Encode.Value
bakeBody { body, params } =
    case parseString body |> Result.andThen Interface.JsonTree.parseValue of
        Err _ ->
            Nothing

        Ok tree ->
            params
                |> List.foldl (\param tree_ -> injectValueInTree param tree_) tree.value
                |> Interface.JsonTree.Model.encodeTree
                |> Just


injectValueInTree : ApiParam -> Interface.JsonTree.Model.TaggedValue -> Interface.JsonTree.Model.TaggedValue
injectValueInTree ({ keyPath, variable, refinedType, data } as apiParam) valueInTree =
    let
        valueToInject =
            data
    in
    case ( keyPath, valueInTree ) of
        ( (Interface.JsonTree.Model.ObjectAccessor str) :: deeperPath, Interface.JsonTree.Model.TDict dict ) ->
            Dict.update str (Maybe.map (\node -> { node | value = injectValueInTree { apiParam | keyPath = deeperPath } node.value })) dict
                |> Interface.JsonTree.Model.TDict

        ( (Interface.JsonTree.Model.IndexAccessor index) :: deeperPath, Interface.JsonTree.Model.TList list ) ->
            List.Extra.updateAt index (\node -> { node | value = injectValueInTree { apiParam | keyPath = deeperPath } node.value }) list
                |> Interface.JsonTree.Model.TList

        ( [], targetLeaf ) ->
            case valueToInject of
                Dynamic.Data.StringInstance stringToInject ->
                    case ( variable, refinedType, targetLeaf ) of
                        -- not an interpolation
                        ( Nothing, _, _ ) ->
                            Interface.JsonTree.Model.TString stringToInject

                        -- interpolation settings, template, data available
                        ( Just { key }, Interface.Selection.Text interpolationSettings, Interface.JsonTree.Model.TString template ) ->
                            let
                                allSettings =
                                    findVariableSlots template interpolationSettings
                                        |> Interface.Selection.interpolationSettingsFromList
                            in
                            case Interface.Selection.getVariable key allSettings of
                                Nothing ->
                                    targetLeaf

                                Just { variableName } ->
                                    Interpolator.interpolateOne variableName stringToInject template
                                        |> Interface.JsonTree.Model.TString

                        -- not enough information to complete the operation
                        _ ->
                            targetLeaf

                _ ->
                    -- TODO: Only strings supported so war, type-check before inserting
                    targetLeaf

        ( _, deadEnd ) ->
            deadEnd


view : Persistence.ProjectMeta -> ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
view projectMeta apiSpec =
    let
        urlSection i =
            case apiSpec.kind of
                ApiExplorer.Model.Mock ->
                    ApiExplorer.Mock.topSection projectMeta apiSpec

                ApiExplorer.Model.Curl ->
                    ApiExplorer.Curl.view apiSpec

                ApiExplorer.Model.Api ->
                    viewSection
                        (String.fromInt i ++ ". Enter the REST API URL, select the type of request you want to make and, if required, any URL parameters")
                        [ urlPicker apiSpec ]

        postBodySection i =
            let
                ( isValid, treeExplorer, parameterSummary ) =
                    case parseString apiSpec.requestBody of
                        Err _ ->
                            ( False
                            , Element.none
                            , Element.none
                            )

                        Ok val ->
                            ( True
                            , viewTree (Just ApiExplorer.Msg.SetRequestParameterSelection) apiSpec.requestBodyParams val
                            , viewTable val apiSpec.requestBodyParams
                            )
            in
            viewSection
                (String.fromInt i ++ ". Enter test JSON data for this API. You can then connect this structure to real data to send when an action occurs within your app. You can also interpolate {{values}} in Text fields.")
                [ requestBodyInput apiSpec
                , viewValidCheck isValid
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    , Element.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
                    ]
                    [ Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text "Parsed output, select values to parameterize" ]
                    , treeExplorer |> Element.el Ui.Style.paperShadow
                    ]
                , parameterSummary
                    |> Element.map ApiExplorer.Msg.SetRequestParameterSelection
                ]

        headerSection i =
            viewSection
                (String.fromInt i ++ ". An API request may need custom header values, if you need these add them here")
                [ headerInput apiSpec ]

        resultSection i =
            viewSection
                (String.fromInt i ++ ". Make a call to the API and get needed data from the JSON response.")
                [ viewTreeOrInstructions apiSpec
                , viewDataTable apiSpec
                ]

        sectionsOnGet =
            [ urlSection
            , headerSection
            , resultSection
            ]
                |> List.indexedMap (\i f -> f (i + 1))

        sectionsOnPost =
            [ urlSection
            , postBodySection
            , headerSection
            , resultSection
            ]
                |> List.indexedMap (\i f -> f (i + 1))
    in
    Element.column
        [ Element.width Element.fill
        , Element.scrollbarY
        , Element.spacing 30
        , Element.paddingEach { edges | bottom = 40 }
        ]
        (case ( apiSpec.method, apiSpec.kind ) of
            ( _, ApiExplorer.Model.Mock ) ->
                [ urlSection 1, resultSection 2 ]

            ( _, ApiExplorer.Model.Curl ) ->
                [ urlSection 1 ]

            ( ApiExplorer.Model.Get, _ ) ->
                sectionsOnGet

            ( ApiExplorer.Model.Post, _ ) ->
                sectionsOnPost
        )



---- URL PICKER ----


urlPicker : ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
urlPicker { url, request, kind, method } =
    let
        getRequestButton =
            let
                isGetSelected =
                    method == ApiExplorer.Model.Get
            in
            Ui.Component.activableButton
                (ApiExplorer.Msg.SetRequestMethod ApiExplorer.Model.Get)
                "GET Request"
                isGetSelected

        postRequestButton =
            Ui.Component.activableButton
                (ApiExplorer.Msg.SetRequestMethod ApiExplorer.Model.Post)
                "POST Request"
                (method == ApiExplorer.Model.Post)

        parseUrlButton =
            let
                isParsed =
                    case url of
                        ApiExplorer.Api.UrlParser.Parsed _ ->
                            True

                        ApiExplorer.Api.UrlParser.Raw _ ->
                            False
            in
            Ui.Component.activableButton
                ApiExplorer.Msg.ParseUrlButtonClicked
                (if isParsed then
                    "Clear Params and unlock URL"

                 else
                    "Assign URL Parameters"
                )
                isParsed

        separator =
            Element.el
                [ Element.Border.widthEach { edges | right = 1 }
                , Element.height Element.fill
                ]
                Element.none

        rawInput rawUrl =
            let
                viewInput label placeholderText text =
                    Element.Input.text
                        ([ Element.Font.size 20 ] ++ greyBackgroundStyle)
                        { placeholder =
                            placeholderText
                                |> Element.text
                                |> Element.Input.placeholder []
                                |> Just
                        , text = text
                        , onChange = identity
                        , label = Element.Input.labelHidden label
                        }

                urlInput =
                    Element.column
                        [ Element.width Element.fill ]
                        [ viewInput "URL" "Enter the URL of your API" rawUrl
                            |> Element.map (ApiExplorer.Msg.UrlChanged << ApiExplorer.Api.UrlParser.Raw)
                        , isRawUrlValid url |> viewValidCheck
                        ]
            in
            Element.column
                [ Element.spacing 15
                , Element.width Element.fill
                ]
                [ urlInput
                , if rawUrl == "" then
                    Element.none

                  else
                    Element.row [ Element.spacingXY 20 0 ]
                        [ getRequestButton
                        , postRequestButton
                        , separator
                        , parseUrlButton
                        ]
                ]
    in
    case url of
        ApiExplorer.Api.UrlParser.Raw rawUrl ->
            rawInput rawUrl

        ApiExplorer.Api.UrlParser.Parsed url_ ->
            Element.column
                [ Element.spacing 15
                , Element.width Element.fill
                ]
                [ viewParameterisedUrl url_
                , viewParameters url_
                , Element.row [ Element.spacingXY 20 0 ]
                    [ getRequestButton
                    , postRequestButton
                    , separator
                    , parseUrlButton
                    ]
                ]


isRawUrlValid url =
    case url of
        ApiExplorer.Api.UrlParser.Raw str ->
            case ApiExplorer.Api.UrlParser.parseUrl str of
                Just _ ->
                    True

                Nothing ->
                    False

        ApiExplorer.Api.UrlParser.Parsed _ ->
            True


viewParameterisedUrl : ApiExplorer.Api.UrlParser.ParsedUrl -> Element.Element ApiExplorer.Msg.ApiSpecEdit
viewParameterisedUrl url =
    let
        { protocol, host, port_, path, query } =
            url

        protocolStr =
            ApiExplorer.Api.UrlParser.protocolToString protocol

        coloredText color text_ =
            Element.el
                [ Element.Font.color color ]
                (Element.text text_)

        entryToColoredText color placeholder entry =
            let
                commonAttr =
                    [ Element.mouseOver [ Element.Font.color color ]
                    , Element.pointer
                    ]
            in
            case entry of
                ApiExplorer.Api.UrlParser.AsInInput str ->
                    Element.el
                        ([ Element.Font.color notSelectedColor
                         , Element.Events.onClick (ApiExplorer.Api.UrlParser.Parameterized placeholder str)
                         ]
                            ++ commonAttr
                        )
                        (Element.text str)

                ApiExplorer.Api.UrlParser.Parameterized placeholder_ str ->
                    Element.el
                        ([ Element.Font.color color
                         , Element.Events.onClick (ApiExplorer.Api.UrlParser.AsInInput str)
                         ]
                            ++ commonAttr
                        )
                        (Element.text ("<" ++ placeholder_ ++ ">"))

        notSelectedColor =
            Element.rgb 0 0 0

        queryParameters =
            case url.query of
                [] ->
                    []

                _ ->
                    Element.text "?"
                        :: (url.query
                                |> List.indexedMap
                                    (\i ( key, e ) ->
                                        [ Element.text (key ++ "=")
                                        , entryToColoredText (Element.rgb 0 0 0.6) key e
                                            |> Element.map (ApiExplorer.Api.UrlParser.editQueryEntry url i)
                                        ]
                                    )
                                |> List.intersperse [ Element.text "&" ]
                                |> List.concat
                           )

        fragment =
            case url.fragment of
                Just frag ->
                    [ Element.text "#"
                    , entryToColoredText (Element.rgb 0.6 0 0) "fragment" frag
                        |> Element.map (ApiExplorer.Api.UrlParser.editFragmentEntry url)
                    ]

                Nothing ->
                    [ Element.none ]
    in
    Element.row
        [ Element.width Element.fill
        , Element.Font.size 20
        ]
        ([ coloredText (Element.rgb 0 0 0) protocolStr
         , coloredText (Element.rgb 0 0 0) host
         ]
            ++ (case port_ of
                    Just int ->
                        [ Element.text (":" ++ String.fromInt int) ]

                    Nothing ->
                        []
               )
            ++ (List.indexedMap
                    (\i e ->
                        entryToColoredText (Element.rgb 0 0.6 0) ("path " ++ String.fromInt i) e
                            |> Element.map (ApiExplorer.Api.UrlParser.editPathEntry url i)
                    )
                    url.path
                    |> List.intersperse (Element.text "/")
               )
            ++ queryParameters
            ++ fragment
        )
        |> Element.map (ApiExplorer.Msg.UrlChanged << ApiExplorer.Api.UrlParser.Parsed)


viewParameters : ApiExplorer.Api.UrlParser.ParsedUrl -> Element.Element ApiExplorer.Msg.ApiSpecEdit
viewParameters url =
    let
        rmEntry entry =
            case entry of
                ApiExplorer.Api.UrlParser.Parameterized placeholder str ->
                    Element.el
                        ([ Element.Events.onClick (ApiExplorer.Api.UrlParser.AsInInput str)
                         , Element.Font.size 20
                         , Element.pointer
                         , Element.mouseOver
                            [ Element.Background.color Ui.Style.black
                            , Element.Font.color Ui.Style.white
                            ]
                         ]
                            ++ greyBackgroundStyle
                        )
                        (Element.text "-")

                ApiExplorer.Api.UrlParser.AsInInput _ ->
                    Element.none

        viewEntry :
            String
            -> ApiExplorer.Api.UrlParser.Entry
            -> Element.Element ApiExplorer.Api.UrlParser.Entry
        viewEntry placeholderText entry =
            case entry of
                ApiExplorer.Api.UrlParser.Parameterized placeholder str ->
                    Element.row
                        [ Element.width Element.fill
                        , Element.spacing 10
                        ]
                        [ Element.Input.text
                            ([ Element.Font.size 20
                             , Element.width <| Element.px 250
                             ]
                                ++ greyBackgroundStyle
                            )
                            { placeholder =
                                (placeholderText ++ " placeholder")
                                    |> Element.text
                                    |> Element.Input.placeholder []
                                    |> Just
                            , text = placeholder
                            , onChange = \newPla -> ApiExplorer.Api.UrlParser.Parameterized newPla str
                            , label = Element.Input.labelHidden placeholderText
                            }
                        , Element.Input.text
                            ([ Element.Font.size 20 ] ++ greyBackgroundStyle)
                            { placeholder =
                                placeholderText
                                    |> Element.text
                                    |> Element.Input.placeholder []
                                    |> Just
                            , text = str
                            , onChange = ApiExplorer.Api.UrlParser.Parameterized placeholder
                            , label = Element.Input.labelHidden placeholderText
                            }
                        , rmEntry entry
                        ]

                ApiExplorer.Api.UrlParser.AsInInput _ ->
                    Element.none

        section color text isEmpty content =
            if isEmpty then
                []

            else
                Element.el
                    [ Element.Font.bold
                    , Element.Font.color color
                    ]
                    (Element.text text)
                    :: content

        isAsInInput e =
            case e of
                ApiExplorer.Api.UrlParser.AsInInput _ ->
                    True

                _ ->
                    False
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 15
        ]
        (let
            instructions =
                Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text "Click parts of the URL to parameterize them" ]

            pathSection =
                let
                    isEmpty =
                        url.path
                            |> List.map isAsInInput
                            |> List.any ((==) False)
                            |> not
                in
                section
                    (Element.rgb 0 0.6 0)
                    "Path"
                    isEmpty
                    (url.path
                        |> List.indexedMap
                            (\i e ->
                                viewEntry ("path " ++ String.fromInt i) e
                                    |> Element.map (ApiExplorer.Api.UrlParser.editPathEntry url i)
                            )
                    )

            querySection =
                let
                    isEmpty =
                        url.query
                            |> List.map (Tuple.second >> isAsInInput)
                            |> List.any ((==) False)
                            |> not
                in
                section
                    (Element.rgb 0 0 0.6)
                    "Query"
                    isEmpty
                    (url.query
                        |> List.indexedMap
                            (\i ( key, e ) ->
                                viewEntry key e
                                    |> Element.map (ApiExplorer.Api.UrlParser.editQueryEntry url i)
                            )
                    )

            fragmentSection =
                case url.fragment of
                    Nothing ->
                        []

                    Just frag ->
                        section
                            (Element.rgb 0.6 0 0)
                            "Fragment"
                            (isAsInInput frag)
                            [ viewEntry "fragment" frag
                                |> Element.map (ApiExplorer.Api.UrlParser.editFragmentEntry url)
                            ]
         in
         instructions
            :: pathSection
            ++ querySection
            ++ fragmentSection
        )
        |> Element.map (ApiExplorer.Msg.UrlChanged << ApiExplorer.Api.UrlParser.Parsed)



---- DATA ----


requestBodyInput model =
    ApiExplorer.Help.multilineInput
        { onChange = ApiExplorer.Msg.UpdateRequestBody
        , text = model.requestBody
        , placeholder =
            Element.text
                "Enter your JSON data here."
                |> Element.Input.placeholder []
                |> Just
        , label = Element.Input.labelHidden "Input test data to send to the server."
        , spellcheck = False
        }


parseString =
    Decode.decodeString Decode.value


isValidHeader ( key, val ) =
    String.isEmpty key || String.isEmpty val |> not



---- Headers ----


{-| Allow the users to add or remove headers to a request
-}
headerInput : ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
headerInput model =
    let
        headerEntry index ( key, val ) =
            Element.row
                [ Element.spacing 5
                , Element.width Element.shrink
                ]
                [ Element.Input.text
                    inputStyle
                    { onChange = ApiExplorer.Msg.UpdateHeaderKey index
                    , text = key
                    , placeholder = toPlaceholder "key"
                    , label = Element.Input.labelHidden "header key"
                    }
                , Element.text " : "
                , Element.Input.text
                    inputStyle
                    { onChange = ApiExplorer.Msg.UpdateHeaderVal index
                    , text = val
                    , placeholder = toPlaceholder "value"
                    , label = Element.Input.labelHidden "header val"
                    }
                , rmEntry index key
                , viewValidCheck (isValidHeader ( key, val ))
                ]

        rmEntry index key =
            Element.el
                ([ Element.Events.onClick (ApiExplorer.Msg.RemoveHeaderKeyVal index)
                 , Element.Font.size 20
                 , Element.pointer
                 , Element.mouseOver
                    [ Element.Background.color Ui.Style.black
                    , Element.Font.color Ui.Style.white
                    ]
                 ]
                    ++ greyBackgroundStyle
                )
                (Element.text "-")

        addEntry =
            Element.el
                ([ Element.Events.onClick ApiExplorer.Msg.AddHeaderKeyVal
                 , Element.Font.size 20
                 , Element.pointer
                 , Element.mouseOver
                    [ Element.Background.color Ui.Style.black
                    , Element.Font.color Ui.Style.white
                    ]
                 ]
                    ++ greyBackgroundStyle
                )
                (Element.text "+")

        headerEntries =
            (List.indexedMap headerEntry model.headers
                |> List.reverse
            )
                ++ [ addEntry ]

        --utils
        inputStyle =
            [ Element.width (Element.minimum 300 Element.shrink)
            , Element.Font.size 20
            ]
                ++ greyBackgroundStyle

        toPlaceholder =
            Element.text
                >> Element.Input.placeholder []
                >> Just
    in
    Element.column
        [ Element.width Element.fill
        , Element.scrollbarY
        , Element.scrollbarX
        , Element.spacing 5
        ]
        headerEntries


{-| Because we have a recursive tree structure that needs conversion into a flat list of table rows
each table row already contains its view
-}
type alias SelectionRow msg =
    { pathColumn : Element.Element msg, typeColumn : Element.Element msg, nameColumn : Element.Element msg }


viewSelectionEntriesAsTableRows :
    Interface.JsonTree.Model.KeyPathList
    -> Decode.Value
    -> Interface.Selection.Selection
    -> List (SelectionRow ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation ))
viewSelectionEntriesAsTableRows contextPath data selection =
    case Dict.Any.isEmpty selection of
        False ->
            selection
                |> Dict.Any.toList
                |> List.map (viewSelectedPathFlat contextPath data)
                |> List.concat

        True ->
            let
                depth =
                    List.length contextPath
            in
            [ { pathColumn =
                    Ui.Table2.textCellNoLine "Please make a selection"
                        |> Element.el [ Element.paddingEach { edges | left = depth * 20 } ]
              , typeColumn = Element.none
              , nameColumn = Element.none
              }
            ]


stringInterpolationSettings :
    Int
    -> Interface.JsonTree.Model.KeyPath
    -> Maybe Interface.Selection.RefinedType
    -> Interface.JsonTree.Model.TaggedValue
    ->
        List
            { nameColumn :
                Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
            , pathColumn :
                Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
            , typeColumn :
                Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
            }
stringInterpolationSettings depth keyPath stringKind data =
    case data of
        Interface.JsonTree.Model.TString str ->
            case stringKind of
                Just (Interface.Selection.Text interpolationTypeSettings) ->
                    let
                        rows =
                            findVariableSlots str interpolationTypeSettings

                        viewInterpolation :
                            ( Interface.Selection.InterpolationVariableKey, Interface.Selection.InterpolationVariableSettings )
                            ->
                                { nameColumn :
                                    Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
                                , pathColumn :
                                    Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
                                , typeColumn :
                                    Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
                                }
                        viewInterpolation ( id, { variableName, kind } ) =
                            { typeColumn = typeSelector id variableName kind
                            , pathColumn =
                                Ui.Table2.textCell ("{{" ++ variableName ++ "}}")
                                    |> Element.el [ Ui.Style.monospace, Element.paddingEach { edges | left = (depth + 1) * 20 }, Element.width Element.fill ]
                            , nameColumn = Element.none
                            }

                        typeSelector :
                            Interface.Selection.InterpolationVariableKey
                            -> String
                            -> Interface.Selection.RefinedType
                            -> Element.Element ( List Interface.JsonTree.Model.KeyPath, ApiExplorer.Msg.EditOperation )
                        typeSelector id variableName refinedTypeOfTemplateFragment =
                            { refinedType = Just refinedTypeOfTemplateFragment, val = data, options = Interface.Selection.refinementOptionsForString }
                                |> viewRefinementOptions
                                |> Element.map (\newType -> Interface.Selection.setInTemplate id { variableName = variableName, kind = newType } interpolationTypeSettings |> Interface.Selection.Text)
                                |> Element.map (ApiExplorer.Msg.SetRefinedType >> Tuple.pair [ keyPath ])
                    in
                    rows
                        |> List.map viewInterpolation

                _ ->
                    []

        _ ->
            []


findAllSlots :
    ApiExplorer.Model.ApiSpec
    -> Interface.JsonTree.Model.KeyPath
    -> Interface.Selection.TemplateRefinements
    -> List ( Maybe Interface.Selection.InterpolationVariableKey, Interface.Selection.InterpolationVariableSettings )
findAllSlots apiSpec keyPath textSlots =
    let
        value =
            case apiSpec.method of
                ApiExplorer.Model.Post ->
                    Decode.decodeString Decode.value apiSpec.requestBody
                        |> Result.toMaybe

                _ ->
                    Nothing

        allSlots =
            case
                Maybe.andThen
                    (Interface.Data.getFromValueAndTag keyPath)
                    value
            of
                Just (Interface.JsonTree.Model.TString str) ->
                    case findVariableSlots str textSlots of
                        [] ->
                            []

                        variableSlots ->
                            variableSlots
                                |> List.map (Tuple.mapFirst Just)

                _ ->
                    []
    in
    allSlots


findVariableSlots :
    String
    -> Interface.Selection.TemplateRefinements
    -> List ( Interface.Selection.InterpolationVariableKey, Interface.Selection.InterpolationVariableSettings )
findVariableSlots str interpolationTypeSettings =
    let
        foundVariables : List String
        foundVariables =
            Interpolator.getVariables str

        -- including variables that were found in the string but not set in the settings
        interpolationTypeSettingsWithOthers =
            Interface.Selection.compileInterpolationSettings
                foundVariables
                interpolationTypeSettings

        rows =
            interpolationTypeSettingsWithOthers
                |> Interface.Selection.interpolationSettingsToList
    in
    rows


{-| View a selected path and the children below it

Spit it out as a row fitting for the table

-}
viewSelectedPathFlat :
    Interface.JsonTree.Model.KeyPathList
    -> Decode.Value
    -> ( Interface.JsonTree.Model.KeyPath, Interface.Selection.SelectedEntrySettings )
    -> List (SelectionRow ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation ))
viewSelectedPathFlat contextPath value ( keyPath, selectedEntrySettings ) =
    let
        depth =
            List.length contextPath

        ( refinements, selectionsForEachItemInListScope ) =
            case selectedEntrySettings.kind of
                Interface.Selection.Single refinedType ->
                    let
                        -- if we are inside a list context this path points to the first item in the list
                        pathToFirstValue =
                            contextPath
                                ++ [ keyPath ]
                                |> List.intersperse [ Interface.JsonTree.Model.IndexAccessor 0 ]
                                |> List.concat

                        refinementAndStringInterpolation =
                            case ( Interface.Data.getFromValueAndTag pathToFirstValue value, selectedEntrySettings.kind ) of
                                ( Just data, Interface.Selection.Single kind ) ->
                                    let
                                        refinements_ =
                                            viewRefinementOptions { refinedType = kind, val = data, options = Interface.Selection.getRefinementOptions data }
                                                |> Element.map (ApiExplorer.Msg.SetRefinedType >> Tuple.pair [ keyPath ])
                                    in
                                    ( refinements_, stringInterpolationSettings depth keyPath kind data )

                                _ ->
                                    ( Element.none, [] )
                    in
                    refinementAndStringInterpolation

                Interface.Selection.List selectionForEachItemInListScope ->
                    let
                        refinementSelector =
                            Ui.Table2.textCell "List"

                        subItems =
                            viewSelectionEntriesAsTableRows (contextPath ++ [ keyPath ]) value selectionForEachItemInListScope
                                |> List.map
                                    (\row ->
                                        { row
                                            | nameColumn =
                                                Element.map (prependPath keyPath)
                                                    row.nameColumn
                                            , pathColumn =
                                                Element.map (prependPath keyPath)
                                                    row.pathColumn
                                            , typeColumn =
                                                Element.map (prependPath keyPath)
                                                    row.typeColumn
                                        }
                                    )

                        prependPath :
                            Interface.JsonTree.Model.KeyPath
                            -> ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )
                            -> ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )
                        prependPath pathToPrepend ( path, editOp ) =
                            ( pathToPrepend :: path, editOp )
                    in
                    ( refinementSelector, subItems )

        pathColumn : Element.Element ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )
        pathColumn =
            let
                path =
                    Ui.Table2.textCell <|
                        case Interface.JsonTree.Model.keyPathToString keyPath of
                            "" ->
                                "[[root]]"

                            notRoot ->
                                notRoot
            in
            Element.el
                [ Ui.Style.monospace
                , Element.width Element.fill
                , Element.paddingEach { edges | left = depth * 20 }
                ]
                path

        typeColumn : Element.Element ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )
        typeColumn =
            refinements

        nameColumn : Element.Element ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )
        nameColumn =
            Element.row
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                (nameInputAndDeleteButton keyPath selectedEntrySettings.name)
    in
    { pathColumn = pathColumn
    , typeColumn = typeColumn
    , nameColumn = nameColumn
    }
        :: selectionsForEachItemInListScope


nameInputAndDeleteButton :
    Interface.JsonTree.Model.KeyPath
    -> String
    -> List (Element.Element ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation ))
nameInputAndDeleteButton keyPath name =
    let
        removeButton =
            Ui.Table2.button
                (Ui.Table2.icon Ui.Boxicons.bxTrash)
                ( [ keyPath ], ApiExplorer.Msg.Deselect )

        nameInput : Element.Element ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )
        nameInput =
            Ui.Table2.input { placeholder = "unnamed field", text = name }
                |> Element.map (ApiExplorer.Msg.SetName >> Tuple.pair [ keyPath ])
    in
    [ nameInput, removeButton ]


{-| In case of a string it is possible that we need to specify it is a URL to an Image or an ID etc.

De-stringly-type things :)

-}
viewRefinementOptions :
    { refinedType : Maybe Interface.Selection.RefinedType
    , val : Interface.JsonTree.Model.TaggedValue
    , options : List Interface.Selection.RefinedType
    }
    -> Element.Element Interface.Selection.RefinedType
viewRefinementOptions { refinedType, val, options } =
    case options of
        [] ->
            Element.el [ Element.alpha 0.5 ] (Element.text "Unsupported Type")

        opts ->
            let
                rows =
                    List.map viewRow opts

                viewRow type_ =
                    Ui.Dropdown.viewRow
                        { isSelected = Just type_ == refinedType
                        , sideNote = Ui.Dropdown.NoDetail
                        , detail = Ui.Dropdown.Description ""
                        , label =
                            Interface.Selection.refinedTypeToString type_
                                |> Ui.Dropdown.Description
                        , onSelect = type_
                        , rightHandText = Nothing
                        }
            in
            Ui.Table2.dropdown
                { label =
                    Maybe.map Interface.Selection.refinedTypeToString refinedType
                        |> Maybe.withDefault "Select type..."
                , contents = rows
                }



---- TREE VIEW ----


{-| Render a schematic view of the JSON data if available
-}
viewTreeOrInstructions : ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
viewTreeOrInstructions model =
    case model.request of
        RemoteData.NotAsked ->
            -- Element.text "Enter a URL or upload a JSON file to make a request."
            Element.none

        RemoteData.Success json ->
            viewTree
                (Just ApiExplorer.Msg.SetResponseDataSelection)
                model.responseDataSelection
                json
                |> Element.el Ui.Style.paperShadow

        RemoteData.Failure reason ->
            Element.column []
                [ Element.text "Something went wrong. Double check your URL and network connection!"
                , case reason of
                    Http.BadUrl url ->
                        Element.text <| "Bad Url: " ++ url

                    Http.Timeout ->
                        Element.text "Timeout"

                    Http.NetworkError ->
                        Element.text "NetworkError"

                    Http.BadStatus x ->
                        Element.text "BadStatus"

                    Http.BadBody _ ->
                        Element.text "BadBody"
                ]

        RemoteData.Loading ->
            Element.text "....."


{-| Render the tree
-}
viewTree :
    Maybe (Interface.Selection.Selection -> msg)
    -> Interface.Selection.Selection
    -> Decode.Value
    -> Element.Element msg
viewTree onChange selection value =
    let
        render :
            Interface.JsonTree.Model.Node
            -> Element.Element msg
        render tree =
            Interface.JsonTree.view
                selection
                tree
                config
                Interface.JsonTree.defaultState

        config : Interface.JsonTree.Config msg
        config =
            { handlers =
                case onChange of
                    Nothing ->
                        Nothing

                    Just changeHandler ->
                        Just
                            { onSelect =
                                selectData selection
                                    >> changeHandler
                            , onDeselect =
                                \select ->
                                    case select of
                                        Interface.JsonTree.Model.Value node ->
                                            Dict.Any.remove
                                                node.keyPath
                                                selection
                                                |> changeHandler

                                        Interface.JsonTree.Model.List keyPath ->
                                            Dict.Any.remove
                                                keyPath
                                                selection
                                                |> changeHandler
                            }
            , toMsg = Nothing
            , colors = Interface.JsonTree.defaultColors
            }

        viewTreeStyles =
            [ Element.padding 20
            , Element.width Element.fill
            , Element.Background.color Ui.Style.white
            , Element.Font.color Ui.Style.black
            , Element.Border.rounded 5
            , Element.clipX
            , Element.scrollbarY
            , Element.height (Element.fill |> Element.maximum 500)
            ]
    in
    value
        |> Interface.JsonTree.parseValue
        |> Result.map render
        |> Result.withDefault (Element.text "Failed to parse JSON")
        |> Element.el viewTreeStyles


viewTable :
    Decode.Value
    -> Interface.Selection.Selection
    -> Element.Element Interface.Selection.Selection
viewTable data selection =
    let
        applyUpdate : ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation ) -> Interface.Selection.Selection
        applyUpdate ( path, operation ) =
            updateRecursive
                (performEdit operation)
                path
                selection

        tableRows =
            viewSelectionEntriesAsTableRows [] data selection

        tableSettings :
            { columns : List (Element.Column (SelectionRow ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation )) ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation ))
            , data : List (SelectionRow ( Interface.JsonTree.Model.KeyPathList, ApiExplorer.Msg.EditOperation ))
            }
        tableSettings =
            { columns =
                [ { header = Ui.Table2.headerText "path"
                  , view = .pathColumn
                  , width = Element.shrink
                  }
                , { header = Ui.Table2.headerText "type"
                  , view = .typeColumn
                  , width = Element.shrink
                  }
                , { header = Ui.Table2.headerText "name"
                  , view = .nameColumn
                  , width = Element.shrink
                  }
                ]
            , data = tableRows
            }
    in
    Ui.Table2.view
        tableSettings
        |> Element.map applyUpdate



---- Request and Data table ----


viewDataTable : ApiExplorer.Model.ApiSpec -> Element.Element ApiExplorer.Msg.ApiSpecEdit
viewDataTable model =
    let
        method =
            case model.method of
                ApiExplorer.Model.Post ->
                    "POST"

                ApiExplorer.Model.Get ->
                    "GET"
    in
    case model.request of
        RemoteData.Success value ->
            Element.column
                [ Element.spacing 15
                , Element.paddingEach { edges | top = 15 }
                ]
                [ viewTable value model.responseDataSelection
                    |> Element.map ApiExplorer.Msg.SetResponseDataSelection
                , Ui.Component.activableButton
                    ApiExplorer.Msg.MakeRequestButtonClicked
                    ("Re-" ++ method ++ " request")
                    True
                ]

        _ ->
            Ui.Component.activableButton
                ApiExplorer.Msg.MakeRequestButtonClicked
                ("Make " ++ method ++ " request")
                True


selectData selection s =
    case s of
        Interface.JsonTree.Model.List keyPath ->
            Dict.Any.insert
                keyPath
                { kind = Interface.Selection.List ApiExplorer.Model.emptySelection
                , name = Interface.JsonTree.Model.keyPathToString keyPath
                }
                selection

        Interface.JsonTree.Model.Value node ->
            let
                guessedType =
                    guessRefinedType node

                -- try updating a list with a subselection
                list =
                    Dict.Any.toList selection
                        -- find out if selected list is a subpath of the selected node
                        |> findMap
                            (\( listKeyPath, { kind, name } ) ->
                                case ( kind, List.Extra.isPrefixOf listKeyPath node.keyPath ) of
                                    ( Interface.Selection.List selectionForEachItemInListScope, True ) ->
                                        Just ( listKeyPath, name, selectionForEachItemInListScope )

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.map
                            (\( listKeyPath, name, selectionForEachItemInListScope ) ->
                                let
                                    -- drop path to list plus index accessor
                                    stepsToDrop =
                                        List.length listKeyPath + 1

                                    subKeyPath =
                                        List.drop stepsToDrop node.keyPath
                                in
                                ( listKeyPath
                                , { name = name
                                  , kind =
                                        Interface.Selection.List <|
                                            Dict.Any.insert subKeyPath
                                                { kind = Interface.Selection.Single guessedType
                                                , name = Interface.JsonTree.Model.keyPathToString node.keyPath
                                                }
                                                selectionForEachItemInListScope
                                  }
                                )
                            )

                newModel =
                    case list of
                        Just ( listKeyPath, newListSelection ) ->
                            Dict.Any.insert
                                listKeyPath
                                newListSelection
                                selection

                        Nothing ->
                            Dict.Any.insert
                                node.keyPath
                                { kind = Interface.Selection.Single guessedType
                                , name = Interface.JsonTree.Model.keyPathToString node.keyPath
                                }
                                selection
            in
            newModel


update : ApiExplorer.Msg.ApiSpecEdit -> ApiExplorer.Model.ApiSpec -> ( ApiExplorer.Model.ApiSpec, Cmd ApiExplorer.Msg.ApiSpecEdit )
update msg apiSpec =
    case msg of
        ApiExplorer.Msg.UpdateCurl curlModel ->
            ( { apiSpec | curl = curlModel }, Cmd.none )

        ApiExplorer.Msg.ApplyCurlButtonClicked curlParseResult ->
            ( ApiExplorer.Model.applyCurlResult curlParseResult apiSpec
            , Cmd.none
            )

        ApiExplorer.Msg.MockFileUploaded mockUrl ->
            ( { apiSpec | mockUrl = mockUrl }, Cmd.none )

        ApiExplorer.Msg.EditDataSelection path operation ->
            ( { apiSpec
                | responseDataSelection =
                    updateRecursive
                        (performEdit operation)
                        path
                        apiSpec.responseDataSelection
              }
            , Cmd.none
            )

        ApiExplorer.Msg.SelectData (Interface.JsonTree.Model.List keyPath) ->
            let
                newModel =
                    { apiSpec
                        | responseDataSelection =
                            Dict.Any.insert
                                keyPath
                                { kind = Interface.Selection.List ApiExplorer.Model.emptySelection
                                , name = Interface.JsonTree.Model.keyPathToString keyPath
                                }
                                apiSpec.responseDataSelection
                    }
            in
            ( newModel
            , Cmd.none
            )

        ApiExplorer.Msg.SelectData (Interface.JsonTree.Model.Value valueNode) ->
            selectValue apiSpec valueNode

        ApiExplorer.Msg.SetDataTreeViewState state ->
            ( apiSpec, Cmd.none )

        ApiExplorer.Msg.UrlChanged url ->
            ( { apiSpec | url = url }, Cmd.none )

        ApiExplorer.Msg.GotResponse response ->
            ( { apiSpec | request = response }, Cmd.none )

        ApiExplorer.Msg.ClearResponse ->
            ( { apiSpec | request = RemoteData.NotAsked }, Cmd.none )

        -- with side-effects
        ApiExplorer.Msg.SetRequestMethod requestedType ->
            { apiSpec | method = requestedType }
                |> update ApiExplorer.Msg.MakeRequestButtonClicked

        ApiExplorer.Msg.MakeRequestButtonClicked ->
            case apiSpec.kind of
                ApiExplorer.Model.Api ->
                    ( { apiSpec | request = RemoteData.Loading }
                    , methodToRequest apiSpec
                    )

                ApiExplorer.Model.Curl ->
                    ( { apiSpec | request = RemoteData.Loading }
                    , methodToRequest apiSpec
                    )

                ApiExplorer.Model.Mock ->
                    ( { apiSpec | request = RemoteData.Loading }
                    , ApiExplorer.Mock.get apiSpec.mockUrl
                    )

        ApiExplorer.Msg.UpdateRequestBody newTestData ->
            ( { apiSpec | requestBody = newTestData }, Cmd.none )

        ApiExplorer.Msg.UpdateHeaderKey index newKey ->
            let
                updateHeaders runIndex ( key, val ) =
                    if runIndex == index then
                        ( newKey, val )

                    else
                        ( key, val )

                newHeaders =
                    List.indexedMap updateHeaders apiSpec.headers
            in
            ( { apiSpec | headers = newHeaders }, Cmd.none )

        ApiExplorer.Msg.UpdateHeaderVal index newVal ->
            let
                updateHeaders runIndex ( key, val ) =
                    if runIndex == index then
                        ( key, newVal )

                    else
                        ( key, val )

                newHeaders =
                    List.indexedMap updateHeaders apiSpec.headers
            in
            ( { apiSpec | headers = newHeaders }, Cmd.none )

        ApiExplorer.Msg.RemoveHeaderKeyVal index ->
            let
                updateHeaders runIndex ( key, val ) =
                    if runIndex == index then
                        Nothing

                    else
                        Just ( key, val )

                newHeaders =
                    List.indexedMap updateHeaders apiSpec.headers
                        |> List.filterMap identity
            in
            ( { apiSpec | headers = newHeaders }, Cmd.none )

        ApiExplorer.Msg.AddHeaderKeyVal ->
            let
                newHeaders =
                    ( "", "" ) :: apiSpec.headers
            in
            ( { apiSpec | headers = newHeaders }, Cmd.none )

        ApiExplorer.Msg.ParseUrlButtonClicked ->
            case apiSpec.url of
                ApiExplorer.Api.UrlParser.Parsed url ->
                    let
                        str =
                            ApiExplorer.Api.UrlParser.toString url
                    in
                    ( { apiSpec | url = ApiExplorer.Api.UrlParser.Raw str }, Cmd.none )

                ApiExplorer.Api.UrlParser.Raw str ->
                    let
                        maybeUrl =
                            ApiExplorer.Api.UrlParser.parseUrl str
                    in
                    case maybeUrl of
                        Just url ->
                            ( { apiSpec | url = ApiExplorer.Api.UrlParser.Parsed url }, Cmd.none )

                        Nothing ->
                            ( apiSpec, Cmd.none )

        ApiExplorer.Msg.SetResponseDataSelection selection ->
            ( { apiSpec | responseDataSelection = selection }, Cmd.none )

        ApiExplorer.Msg.SetRequestParameterSelection selection ->
            ( { apiSpec | requestBodyParams = selection }, Cmd.none )


selectValue model valueNode =
    let
        node =
            valueNode

        guessedType =
            guessRefinedType node

        -- try updating a list with a subselection
        list =
            Dict.Any.toList model.responseDataSelection
                -- find out if selected list is a subpath of the selected node
                |> findMap
                    (\( listKeyPath, { kind, name } ) ->
                        case ( kind, List.Extra.isPrefixOf listKeyPath node.keyPath ) of
                            ( Interface.Selection.List selectionForEachItemInListScope, True ) ->
                                Just ( listKeyPath, name, selectionForEachItemInListScope )

                            _ ->
                                Nothing
                    )
                |> Maybe.map
                    (\( listKeyPath, name, selectionForEachItemInListScope ) ->
                        let
                            -- drop path to list plus index accessor
                            stepsToDrop =
                                List.length listKeyPath + 1

                            subKeyPath =
                                List.drop stepsToDrop node.keyPath
                        in
                        ( listKeyPath
                        , { name = name
                          , kind =
                                Interface.Selection.List <|
                                    Dict.Any.insert subKeyPath
                                        { kind = Interface.Selection.Single guessedType
                                        , name = Interface.JsonTree.Model.keyPathToString node.keyPath
                                        }
                                        selectionForEachItemInListScope
                          }
                        )
                    )

        newModel =
            case list of
                Just ( listKeyPath, newListSelection ) ->
                    { model
                        | responseDataSelection =
                            Dict.Any.insert
                                listKeyPath
                                newListSelection
                                model.responseDataSelection
                    }

                Nothing ->
                    { model
                        | responseDataSelection =
                            Dict.Any.insert
                                node.keyPath
                                { kind = Interface.Selection.Single guessedType
                                , name = Interface.JsonTree.Model.keyPathToString node.keyPath
                                }
                                model.responseDataSelection
                    }
    in
    ( newModel
    , Cmd.none
    )


guessRefinedType : Interface.JsonTree.Model.Node -> Maybe Interface.Selection.RefinedType
guessRefinedType node =
    case node.value of
        Interface.JsonTree.Model.TString str ->
            if String.endsWith ".jpg" str then
                Just Interface.Selection.Image

            else if String.endsWith ".png" str then
                Just Interface.Selection.Image

            else
                -- TODO: guess refined types of templates by name here?
                Just (Interface.Selection.Text Interface.Selection.emptyTemplateRefinements)

        Interface.JsonTree.Model.TFloat float ->
            let
                isInt =
                    if String.contains "." (String.fromFloat float) then
                        False

                    else
                        True
            in
            case isInt of
                True ->
                    Just Interface.Selection.Int

                False ->
                    Just Interface.Selection.Float

        Interface.JsonTree.Model.TBool bool ->
            Just Interface.Selection.Bool

        _ ->
            Nothing


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap fn =
    List.filterMap fn
        >> List.head


{-| Update a selection recursively
-}
updateRecursive :
    (Maybe Interface.Selection.SelectedEntrySettings -> Maybe Interface.Selection.SelectedEntrySettings)
    -> Interface.JsonTree.Model.KeyPathList
    -> Interface.Selection.Selection
    -> Interface.Selection.Selection
updateRecursive fn path selection =
    case path of
        [] ->
            selection

        [ oneLevel ] ->
            Dict.Any.update
                oneLevel
                fn
                selection

        oneLevel :: deeperLevels ->
            Dict.Any.update
                oneLevel
                (\selectionItem ->
                    case selectionItem of
                        Nothing ->
                            Nothing

                        Just s ->
                            case s.kind of
                                Interface.Selection.Single _ ->
                                    selectionItem

                                Interface.Selection.List selectionForEachItemInListScope ->
                                    { s
                                        | kind =
                                            updateRecursive fn deeperLevels selectionForEachItemInListScope
                                                |> Interface.Selection.List
                                    }
                                        |> Just
                )
                selection


performEdit :
    ApiExplorer.Msg.EditOperation
    -> (Maybe Interface.Selection.SelectedEntrySettings -> Maybe Interface.Selection.SelectedEntrySettings)
performEdit operation =
    case operation of
        ApiExplorer.Msg.SetName name ->
            Maybe.map (\settings -> { settings | name = name })

        ApiExplorer.Msg.SetRefinedType refinedType ->
            Maybe.map (\settings -> { settings | kind = Interface.Selection.Single (Just refinedType) })

        ApiExplorer.Msg.Deselect ->
            always Nothing


methodToRequest : ApiExplorer.Model.ApiSpec -> Cmd ApiExplorer.Msg.ApiSpecEdit
methodToRequest model =
    let
        rawUrl =
            case model.url of
                ApiExplorer.Api.UrlParser.Raw str ->
                    str

                ApiExplorer.Api.UrlParser.Parsed url_ ->
                    ApiExplorer.Api.UrlParser.toString url_
    in
    case model.method of
        ApiExplorer.Model.Post ->
            post rawUrl model

        ApiExplorer.Model.Get ->
            get rawUrl model


get : String -> ApiExplorer.Model.ApiSpec -> Cmd ApiExplorer.Msg.ApiSpecEdit
get url { headers } =
    let
        processedHeaders =
            headers
                |> List.filter isValidHeader
                |> List.map (\( k, v ) -> Http.header k v)

        decoder =
            Decode.oneOf
                [ ApiExplorer.Request.readJsonData
                , Decode.value
                ]
    in
    RemoteData.Http.getWithConfig
        { defaultConfig | headers = processedHeaders }
        url
        ApiExplorer.Msg.GotResponse
        decoder


defaultConfig =
    RemoteData.Http.defaultConfig


post : String -> ApiExplorer.Model.ApiSpec -> Cmd ApiExplorer.Msg.ApiSpecEdit
post rawUrl { url, requestBody, headers } =
    let
        decoder =
            Decode.oneOf
                [ ApiExplorer.Request.readJsonData
                , Decode.value
                ]

        encoder =
            case parseString requestBody of
                Ok value ->
                    value

                Err _ ->
                    Encode.null

        processedHeaders =
            headers
                |> List.filter isValidHeader
                |> List.map (\( k, v ) -> Http.header k v)
    in
    RemoteData.Http.postWithConfig
        { defaultConfig | headers = processedHeaders }
        rawUrl
        ApiExplorer.Msg.GotResponse
        decoder
        encoder


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }
