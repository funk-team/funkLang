module Canvas.AttributesPanel.Content.Api exposing (view)

{-| Display the pieces of data from an API that have been pre-selected by a user.
-}

import Canvas.AttributesPanel.Shared
import Dict.Any
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Interface
import Interface.JsonTree.Model
import Interface.Model
import Interface.Selection
import Model.Model
import Ui.Boxicons
import Ui.Component
import Ui.Dropdown
import Ui.Style



{-
   view give the ability to the user to connect api data with some element of his website.
-}


view :
    -- view
    List Interface.Model.ListScopePointer
    -> Maybe Interface.Model.InterfacePointer
    -> Model.Model.UserModel
    -> Element.Element Interface.Model.InterfacePointer
view localOptions currentlyConnectedInterface userModel =
    Element.column
        []
        [ Element.column []
            (localOptions
                |> List.map (viewOptionsProvidedByContextWithHeader currentlyConnectedInterface)
            )
        , viewGlobalOptions currentlyConnectedInterface userModel
        ]



---- VIEW GLOBAL OPTIONS ----


{-| Render a dropdown to select different content sources and the connection options they make available
NOT including the context of lists
-- TODO: move all of this into interface

currently this is a two-step process

1.  select the interface to get the data from
2.  select the data bit from the interface

-}
viewGlobalOptions :
    Maybe Interface.Model.InterfacePointer
    -> Model.Model.UserModel
    -> Element.Element Interface.Model.InterfacePointer
viewGlobalOptions currentlyConnectedInterface userModel =
    let
        -- extract interesting infos from currentlyConnectedInterface
        currentlyConnectedInterfaceId =
            Maybe.map .interfaceId currentlyConnectedInterface

        -- header with a title and the dropdown
        header =
            Element.row
                [ Element.spacing 5, Element.paddingEach { edges | bottom = 5 } ]
                [ Element.el [ Element.Font.bold ] <| Element.text "Api Content"
                , interfaceSelection
                ]

        description : Element.Element msg
        description =
            Element.paragraph [ Element.Font.color Ui.Style.grey ]
                [ Element.text "Connect data from an api to the element of your layout"
                ]

        -- the dropdown to select the interface
        interfaceSelection =
            let
                label =
                    case
                        currentlyConnectedInterfaceId
                            |> Maybe.andThen (\interfaceId -> Interface.getSource interfaceId userModel)
                    of
                        Nothing ->
                            "No source selected"

                        Just { name } ->
                            name

                sourceToDropDownRow : Interface.Model.GenericInterface -> Ui.Dropdown.Row Interface.Model.InterfacePointer
                sourceToDropDownRow { name, id } =
                    Ui.Dropdown.viewRow
                        { isSelected = isSelectedSource id
                        , label = Ui.Dropdown.Description name
                        , detail = Ui.Dropdown.NoDetail
                        , sideNote = Ui.Dropdown.NoDetail
                        , onSelect =
                            { interfaceId = id
                            , kind = Nothing
                            }
                        , rightHandText = Nothing
                        }

                -- recognise if this is the source selected in the dropdown
                isSelectedSource interfaceId =
                    currentlyConnectedInterfaceId
                        |> Maybe.map ((==) interfaceId)
                        |> Maybe.withDefault False
            in
            Ui.Dropdown.view []
                { label =
                    label
                , contents =
                    userModel
                        |> Interface.list
                        |> List.map sourceToDropDownRow
                }

        -- the content of the selected interface
        selectedSourceContent : Element.Element Interface.Model.InterfacePointer
        selectedSourceContent =
            let
                attr =
                    [ Element.paddingEach { edges | top = 15 }
                    , Element.width Element.fill
                    ]

                content =
                    case currentlyConnectedInterfaceId of
                        Nothing ->
                            Element.none

                        Just interfaceId ->
                            case Interface.getSource interfaceId userModel of
                                Just source ->
                                    viewInterface interfaceId source currentlyConnectedInterface

                                Nothing ->
                                    Element.none
            in
            Element.el attr content
    in
    if List.isEmpty <| Interface.list userModel then
        Element.paragraph
            [ Element.padding 15
            , Element.width Element.fill
            , Element.Font.color Ui.Style.grey
            ]
            (Element.text "Use the API or Code tabs above to provide content"
                |> List.singleton
            )

    else
        Element.column
            Canvas.AttributesPanel.Shared.sectionStyles
            [ header
            , description
            , selectedSourceContent
            ]


{-| View all the available entries given by an API source
-}
viewInterface :
    Interface.Model.InterfaceKey
    -> Interface.Model.GenericInterface
    -> Maybe Interface.Model.InterfacePointer
    -> Element.Element Interface.Model.InterfacePointer
viewInterface interfaceId apiCall currentlyConnectedInterface =
    let
        bakeConnection : BakeConnection
        bakeConnection path =
            { rootPath = path }
                |> Interface.Model.RelativeToInterfaceRoot
                |> Just
                |> Interface.Model.InterfacePointer interfaceId

        entries =
            Dict.Any.toList
                apiCall.outputSelection
                |> List.map (viewApiEntry bakeConnection currentlyConnectedInterface)

        singleEntriesOrMessage =
            case entries of
                [] ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.spacing 10
                        ]
                        [ Element.text "Use the API Explorer to connect data"
                        ]

                _ ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing 10
                        , Element.padding 0
                        ]
                        entries
    in
    singleEntriesOrMessage


{-| Render the options from a (list) context including the header
-}
viewOptionsProvidedByContextWithHeader :
    Maybe Interface.Model.InterfacePointer
    -> Interface.Model.ListScopePointer
    -> Element.Element Interface.Model.InterfacePointer
viewOptionsProvidedByContextWithHeader currentlyConnectedInterface { interfaceId, name, selectionForEachItemInListScope, pointerToListInInterface } =
    let
        bakeConnection : BakeConnection
        bakeConnection path =
            Interface.Model.FromListScope
                -- the selection is bound to the context
                -- whereas the context is the path of a connection from root
                -- and in this context we again use a path to get some data
                { subPath = path
                , scopeId = ( interfaceId, pointerToListInInterface.rootPath )
                }
                |> Just
                |> Interface.Model.InterfacePointer interfaceId

        description =
            "The options provided by a parent element which is connected with a list"

        header =
            Element.column [ Element.spacing 15 ]
                [ Element.el [ Element.Font.bold ] (Element.text "Content of Parent List")
                , Element.paragraph [ Element.Font.color Ui.Style.grey ] [ Element.text description ]
                ]

        -- render only the available options
        options =
            viewOptionItemsProvidedByContext
                bakeConnection
                name
                selectionForEachItemInListScope
                currentlyConnectedInterface
    in
    Element.column
        Canvas.AttributesPanel.Shared.sectionStyles
        [ header, options ]


{-| Only the options list
-}
viewOptionItemsProvidedByContext :
    BakeConnection
    -> String
    -> Interface.Selection.Selection
    -> Maybe Interface.Model.InterfacePointer
    -> Element.Element Interface.Model.InterfacePointer
viewOptionItemsProvidedByContext bakeConnection name selectionForEachItemInListScope currentlyConnectedInterface =
    case Dict.Any.toList selectionForEachItemInListScope of
        -- was the subselection empty?
        [] ->
            Element.none

        -- if everything went well, return all the items in the list subselection
        -- and with the name of the list superselection
        listSubSelection ->
            let
                list =
                    Element.row [ Element.width Element.fill ]
                        [ Element.el
                            [ Element.Font.color Ui.Style.grey
                            , Element.height (Element.px 16)
                            , Element.Font.center
                            ]
                            (Ui.Component.icon Ui.Boxicons.bxListUl)
                        , Element.text name
                        ]

                items =
                    Element.el
                        [ Element.paddingEach
                            { edges | left = 10 }
                        , Element.width Element.fill
                        ]
                        (Element.column
                            [ Element.Border.widthEach
                                { edges | left = 1 }
                            , Element.Border.color Ui.Style.grey
                            , Element.spacing 10
                            , Element.paddingEach
                                { edges | left = 2, top = 10 }
                            , Element.width Element.fill
                            ]
                            (List.map
                                (viewApiEntry bakeConnection currentlyConnectedInterface)
                                listSubSelection
                            )
                        )
            in
            Element.column
                [ Element.paddingEach { edges | top = 15 }
                , Element.width Element.fill
                ]
                [ list, items ]



---- VIEW ENTRY ----
{-
   Both viewGlobalOptions and viewLocal options end up rendering entries.
   This is the basic element that serves to select your DataConnection.


   viewSth : Maybe Sth -> Sth -> Element Sth
   viewSth current thisOption =
     let
       sel = Just current == thisOption
     in
       if sel then "selected" else button thisOption "select"

   viewSth maybeActiveBla oneBla |> Element.map SthMsg
-}


type alias BakeConnection =
    Interface.JsonTree.Model.KeyPath -> Interface.Model.InterfacePointer


viewApiEntry :
    BakeConnection
    -> Maybe Interface.Model.InterfacePointer
    -> ( Interface.JsonTree.Model.KeyPath, Interface.Selection.SelectedEntrySettings )
    -> Element.Element Interface.Model.InterfacePointer
viewApiEntry bakeConnection currentlyConnectedInterface ( path, { name, kind } ) =
    let
        isSelected =
            currentlyConnectedInterface == Just apiSource

        apiSource : Interface.Model.InterfacePointer
        apiSource =
            bakeConnection path

        removeSelectedApiSource =
            Element.el
                [ Element.alignRight ]
                (Canvas.AttributesPanel.Shared.viewHeaderButton
                    "-"
                    { apiSource | kind = Nothing }
                )

        dataEntryStyles =
            [ Element.width Element.fill
            , Element.padding 3
            , Element.Border.rounded 2
            ]
                |> List.append
                    (if isSelected then
                        [ Element.Background.color Ui.Style.highlightColor ]

                     else
                        []
                    )

        kindIcon =
            Interface.Selection.kindToIcon kind

        label =
            Element.row
                [ Element.width Element.fill ]
                [ Element.el
                    [ Element.Font.color Ui.Style.grey
                    , Element.height (Element.px 16)
                    , Element.Font.center
                    ]
                    kindIcon
                , wrappedName
                , if isSelected then
                    removeSelectedApiSource

                  else
                    Element.none
                ]

        wrappedName =
            if String.length name < 25 then
                Element.text name

            else
                String.split "." name
                    |> List.filter ((/=) "")
                    |> List.indexedMap
                        (\index str ->
                            if index == 0 then
                                "." ++ str

                            else
                                "   ." ++ str
                        )
                    |> List.map Element.text
                    |> Element.column []
    in
    Element.Input.button dataEntryStyles
        { onPress = Just apiSource
        , label = label
        }



---- UTILS ----


edges =
    { bottom = 0, left = 0, right = 0, top = 0 }
