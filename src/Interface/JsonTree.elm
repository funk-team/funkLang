-- Copyright (c) Microsoft Corporation. All rights reserved.
-- Licensed under the MIT License.


module Interface.JsonTree exposing (..)

{-| This library provides a JSON tree view. You feed it JSON, and it transforms it into
interactive HTML.

Features:

  - show JSON as a tree of HTML
  - expand/collapse nodes in the tree
  - expand/collapse the entire tree
  - select scalar values in the tree


# Basic Usage

@docs parseString, parseValue, view


# Types

@docs Config, State, defaultState, Node, TaggedValue, KeyPath


# Expand/Collapse

@docs expandAll, collapseToDepth


# Customizing Colors

@docs Colors, defaultColors


# Uncommon/Advanced Stuff

**Note:** These are rarely needed, but they are there if you need it.

@docs stateToJson, stateFromJson

-}

import Dict exposing (Dict)
import Dict.Any
import Element
import Html exposing (Attribute, Html, div, li, span, text, ul)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Interface.JsonTree.Model exposing (..)
import Interface.Selection
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set.Any as Set
import Ui.Style


{-| Parse a JSON value as a tree.
-}
parseValue : Decode.Value -> Result Decode.Error Node
parseValue json =
    let
        rootKeyPath : KeyPath
        rootKeyPath =
            []

        decoder =
            Decode.map (annotate rootKeyPath) coreDecoder
    in
    Decode.decodeValue decoder json


{-| Parse a JSON string as a tree.
-}
parseString : String -> Result Decode.Error Node
parseString string =
    Decode.decodeString Decode.value string
        |> Result.andThen parseValue


coreDecoder : Decoder Node
coreDecoder =
    let
        makeNode : TaggedValue -> { value : TaggedValue, keyPath : KeyPath }
        makeNode v =
            { value = v, keyPath = [] }
    in
    Decode.oneOf
        [ Decode.map (makeNode << TString) Decode.string
        , Decode.map (makeNode << TFloat) Decode.float
        , Decode.map (makeNode << TBool) Decode.bool
        , Decode.map (makeNode << TList) (Decode.list (Decode.lazy (\_ -> coreDecoder)))
        , Decode.map (makeNode << TDict) (Decode.dict (Decode.lazy (\_ -> coreDecoder)))
        , Decode.null (makeNode TNull)
        ]


annotate : KeyPath -> Node -> Node
annotate pathSoFar node =
    let
        annotateList index val =
            annotate (pathSoFar ++ [ IndexAccessor index ]) val

        annotateDict fieldName val =
            annotate (pathSoFar ++ [ ObjectAccessor fieldName ]) val
    in
    case node.value of
        TString _ ->
            { node | keyPath = pathSoFar }

        TFloat _ ->
            { node | keyPath = pathSoFar }

        TBool _ ->
            { node | keyPath = pathSoFar }

        TNull ->
            { node | keyPath = pathSoFar }

        TList children ->
            { node
                | keyPath = pathSoFar
                , value = TList (List.indexedMap annotateList children)
            }

        TDict dict ->
            { node
                | keyPath = pathSoFar
                , value = TDict (Dict.map annotateDict dict)
            }


{-| The colors to be used when showing elements of the JSON Tree.
The strings must be valid CSS colors (e.g. "red", "#ff0000").
-}
type alias Colors =
    { string : String
    , number : String
    , bool : String
    , null : String
    , selectable : String
    }


{-| The defaults colors, which are suitable for a light background.
-}
defaultColors : Colors
defaultColors =
    { string = "green"
    , number = "blue"
    , bool = "firebrick"
    , null = "gray"
    , selectable = "#fafad2"
    }


{-| Configuration of the JSON tree view. It describes how to map events in the tree view
into events that your app understands.

Since the `Config` contains functions, it should never be held in your model. It should
only appear in your `view` code.

`colors` may be defaultColors, or another color set

`onSelect` should be set to `Nothing` for most users. However, if you want to make the
tree's leaf nodes selectable, you should provide a function that takes the selected `KeyPath`
and acts on it.

`toMsg` provides an updated `State` to your application which you should use to overwrite
the previous state.

-}
type alias Config msg =
    { colors : Colors
    , handlers : Maybe (Handlers msg)
    , toMsg : Maybe (State -> msg)
    }


type alias Handlers msg =
    { onSelect : Selection -> msg
    , onDeselect : Selection -> msg
    }


{-| Show a JSON tree.
-}
view : Interface.Selection.Selection -> Node -> Config msg -> State -> Element.Element msg
view selection node config state =
    div
        (styleList css.root)
        (hoverStyles config :: viewNodeInternal selection 0 config node state)
        |> Element.html
        |> Element.el [ Ui.Style.monospace, Element.width Element.fill ]


{-| Initial state where the entire tree is fully expanded.
-}
defaultState : State
defaultState =
    stateFullyExpanded


{-| Encodes the state of a tree into JSON.
-}
stateToJson : State -> Encode.Value
stateToJson (State keyPaths) =
    Encode.list encodeKeyPath (Set.toList keyPaths)


{-| Decode the state of a tree from JSON.
-}
stateFromJson : Decoder State
stateFromJson =
    Decode.map
        (newSet >> State)
        (Decode.list decodeKeyPath)


newSet : List KeyPath -> Set.AnySet String KeyPath
newSet =
    Set.fromList keyPathToString


{-| Collapses any nodes deeper than `maxDepth`.
-}
collapseToDepth : Int -> Node -> State -> State
collapseToDepth maxDepth tree _ =
    collapseToDepthHelp maxDepth 0 tree stateFullyExpanded


collapseToDepthHelp : Int -> Int -> Node -> State -> State
collapseToDepthHelp maxDepth currentDepth node state =
    let
        descend children =
            List.foldl
                (collapseToDepthHelp maxDepth (currentDepth + 1))
                (if currentDepth >= maxDepth then
                    collapse node.keyPath state

                 else
                    state
                )
                children
    in
    case node.value of
        TString str ->
            state

        TFloat x ->
            state

        TBool bool ->
            state

        TNull ->
            state

        TList nodes ->
            descend nodes

        TDict dict ->
            descend (Dict.values dict)


{-| Expand all nodes
-}
expandAll : State -> State
expandAll _ =
    stateFullyExpanded


stateFullyExpanded : State
stateFullyExpanded =
    State (Set.fromList keyPathToString [])



-- EXPAND/COLLAPSE --


lazyStateChangeOnClick : (() -> State) -> (State -> msg) -> Attribute msg
lazyStateChangeOnClick newStateThunk toMsg =
    {- This is semantically equivalent to `onClick (toMsg newState)`, but defers the computation
       of the new `State` until the event is delivered/decoded.
    -}
    let
        force =
            \thunk -> thunk ()
    in
    newStateThunk
        |> Decode.succeed
        |> Decode.map (force >> toMsg)
        |> Html.Events.on "click"


expand : KeyPath -> State -> State
expand keyPath ((State hiddenPaths) as state) =
    State (Set.remove keyPath hiddenPaths)


collapse : KeyPath -> State -> State
collapse keyPath ((State hiddenPaths) as state) =
    State (Set.insert keyPath hiddenPaths)


isCollapsed : KeyPath -> State -> Bool
isCollapsed keyPath ((State hiddenPaths) as state) =
    Set.member keyPath hiddenPaths


viewNodeInternal : Interface.Selection.Selection -> Int -> Config msg -> Node -> State -> List (Html msg)
viewNodeInternal selection depth config node state =
    let
        boolToString bool =
            -- workaround for https://github.com/avh4/elm-format/issues/209
            if bool then
                "true"

            else
                "false"

        colors =
            config.colors
    in
    case node.value of
        TString str ->
            viewScalar selection (css.string colors) ("\"" ++ str ++ "\"") node config

        TFloat x ->
            viewScalar selection (css.number colors) (String.fromFloat x) node config

        TBool bool ->
            viewScalar selection (css.bool colors) (boolToString bool) node config

        TNull ->
            viewScalar selection (css.null colors) "null" node config

        TList nodes ->
            [ viewArray selection depth nodes node.keyPath config state ]

        TDict dict ->
            viewDict selection depth dict node.keyPath config state


viewScalar : Interface.Selection.Selection -> List ( String, String ) -> String -> Node -> Config msg -> List (Html msg)
viewScalar selection someCss str node config =
    let
        isSelected =
            case List.filter (\( selectedKeyPath, _ ) -> node.keyPath == selectedKeyPath) (selection |> Dict.Any.toList) of
                [] ->
                    False

                _ ->
                    True

        ( selectButton, handleClick ) =
            case config.handlers of
                Just { onDeselect, onSelect } ->
                    let
                        btn =
                            if isSelected then
                                span
                                    [ onClick (onDeselect (Value node))
                                    ]
                                    [ text "[x] " ]

                            else
                                text ""

                        click =
                            [ onClick (onSelect (Value node))
                            ]
                    in
                    ( btn, click )

                Nothing ->
                    ( Html.text "", [] )

        styles =
            if isSelected then
                [ Html.Attributes.style "background-color" "rgba(0,0,0,0.05)" ]

            else
                []

        render =
            span
                ([ id (keyPathToString node.keyPath), class selectableNodeClass ]
                    ++ styleList someCss
                    ++ handleClick
                    ++ styles
                )
                [ text str
                ]
    in
    [ selectButton, render ]


viewCollapser : Int -> Config msg -> (() -> State) -> String -> Html msg
viewCollapser depth config newStateThunk displayText =
    case config.toMsg of
        Nothing ->
            Html.text ""

        Just toMsg ->
            if depth == 0 then
                text ""

            else
                span
                    (lazyStateChangeOnClick newStateThunk toMsg
                        :: styleList css.collapser
                    )
                    [ text displayText ]


viewExpandButton : Int -> KeyPath -> Config msg -> State -> Html msg
viewExpandButton depth keyPath config state =
    viewCollapser depth config (\_ -> expand keyPath state) "+"


viewCollapseButton : Int -> KeyPath -> Config msg -> State -> Html msg
viewCollapseButton depth keyPath config state =
    viewCollapser depth config (\_ -> collapse keyPath state) "-"


viewArray :
    Interface.Selection.Selection
    -> Int
    -> List Node
    -> KeyPath
    -> Config msg
    -> State
    -> Html msg
viewArray selection depth nodes keyPath config state =
    let
        isSelected =
            case
                List.filter
                    (\( listKeyPath, { kind } ) ->
                        case ( keyPath == listKeyPath, kind ) of
                            ( True, Interface.Selection.List selectionForEachItemInListScope ) ->
                                True

                            _ ->
                                False
                    )
                    (selection |> Dict.Any.toList)
            of
                [] ->
                    False

                _ ->
                    True

        innerContent =
            if List.isEmpty nodes then
                []

            else if isCollapsed keyPath state then
                [ viewExpandButton depth keyPath config state
                , text "…"
                ]

            else
                [ viewCollapseButton depth keyPath config state
                , ul
                    (styleList css.ul)
                    (List.map viewListItem nodes)
                ]

        selectButton =
            case config.handlers of
                Nothing ->
                    Html.text ""

                Just { onSelect, onDeselect } ->
                    if isSelected then
                        span
                            [ onClick (onDeselect (List keyPath))
                            ]
                            [ text "[x]" ]

                    else
                        div
                            [ onClick (onSelect (List keyPath))
                            ]
                            [ text "[select]" ]

        styles =
            if isSelected then
                [ Html.Attributes.style "background-color" "rgba(0,0,0,0.05)" ]

            else
                []

        viewListItem node =
            li
                (styleList css.li)
                (List.append (viewNodeInternal selection (depth + 1) config node state) [ text "," ])

        allElements =
            [ selectButton
            , text "["
            ]
                ++ innerContent
                ++ [ text "]"
                   ]
    in
    div styles allElements


viewDict : Interface.Selection.Selection -> Int -> Dict String Node -> KeyPath -> Config msg -> State -> List (Html msg)
viewDict selection depth dict keyPath config state =
    let
        innerContent =
            if Dict.isEmpty dict then
                []

            else if isCollapsed keyPath state then
                [ viewExpandButton depth keyPath config state
                , text "…"
                ]

            else
                [ viewCollapseButton depth keyPath config state
                , ul
                    (styleList css.ul)
                    (List.map viewListItem (Dict.toList dict))
                ]

        viewListItem ( fieldName, node ) =
            li
                (styleList css.li)
                ([ span (styleList css.fieldName) [ text fieldName ]
                 , text ": "
                 ]
                    ++ viewNodeInternal selection (depth + 1) config node state
                    ++ [ text "," ]
                )
    in
    [ text "{" ] ++ innerContent ++ [ text "}" ]


type alias Css =
    { root : List ( String, String )
    , ul : List ( String, String )
    , li : List ( String, String )
    , collapser : List ( String, String )
    , fieldName : List ( String, String )
    , string : Colors -> List ( String, String )
    , number : Colors -> List ( String, String )
    , bool : Colors -> List ( String, String )
    , null : Colors -> List ( String, String )
    , selectable : Colors -> List ( String, String )
    }


css : Css
css =
    { root =
        [ ( "white-space", "pre" )
        ]
    , ul =
        [ ( "list-style-type", "none" )
        , ( "margin-left", "26px" )
        , ( "padding-left", "0px" )
        ]
    , li =
        [ ( "position", "relative" )
        , ( "margin-top", "5px" )
        ]
    , collapser =
        [ ( "position", "absolute" )
        , ( "cursor", "pointer" )
        , ( "top", "1px" )
        , ( "left", "-15px" )
        ]
    , fieldName =
        [ ( "font-weight", "bold" )
        ]
    , string = \colors -> [ ( "color", colors.string ), ( "white-space", "initial" ) ]
    , number = \colors -> [ ( "color", colors.number ) ]
    , bool = \colors -> [ ( "color", colors.bool ) ]
    , null = \colors -> [ ( "color", colors.null ) ]
    , selectable =
        \colors ->
            [ ( "background-color", colors.selectable )
            , ( "cursor", "pointer" )
            ]
    }


styleList : List ( String, String ) -> List (Html.Attribute msg)
styleList styles =
    List.map (\( name, value ) -> style name value) styles


{-| Inserts a `<style>...</style>` element into the DOM in order to style
CSS pseudo-elements such as hover. This is the technique used by elm-css.
-}
hoverStyles : Config msg -> Html msg
hoverStyles config =
    case config.handlers of
        Nothing ->
            Html.text ""

        Just _ ->
            let
                selectableStyleString =
                    css.selectable config.colors
                        |> List.map (\( name, value ) -> name ++ ": " ++ value ++ ";")
                        |> String.join "\n"

                styleBody =
                    "." ++ selectableNodeClass ++ ":hover {\n" ++ selectableStyleString ++ "\n}\n"
            in
            Html.node "style" [] [ text styleBody ]


selectableNodeClass =
    "selectableJsonTreeNode"
