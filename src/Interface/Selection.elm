module Interface.Selection exposing (..)

{-| Allows the user to select values from a JSONTree and refine them.
-}

import Dict
import Dict.Any
import Dict.Extra
import Element
import IntDict
import Interface.JsonTree.Model
import Json.Decode as Decode
import Json.Encode as Encode
import Ui.Boxicons
import Ui.Component


type alias Selection =
    Dict.Any.AnyDict String Interface.JsonTree.Model.KeyPath SelectedEntrySettings



-- only used to decode Selection


type alias Node =
    Interface.JsonTree.Model.Node

empty = Dict.Any.empty Interface.JsonTree.Model.keyPathToString

getRefinementOptions :
    Interface.JsonTree.Model.TaggedValue
    -> List RefinedType
getRefinementOptions val =
    case val of
        Interface.JsonTree.Model.TString str ->
            Image :: refinementOptionsForString

        Interface.JsonTree.Model.TFloat float ->
            [ Float, Int ]

        Interface.JsonTree.Model.TBool bool ->
            [ Bool ]

        _ ->
            []


refinementOptionsForString =
    [ Text (TemplateRefinements Dict.empty), EmailAddress, FullName ]


refinedTypeToString : RefinedType -> String
refinedTypeToString opt =
    case opt of
        Image ->
            "Image"

        FullName ->
            "Full name"

        EmailAddress ->
            "E-Mail address"

        Text _ ->
            "Text"

        Int ->
            "Int"

        Float ->
            "Float"

        Bool ->
            "Bool"


refinedTypeToIcon : RefinedType -> Element.Element msg
refinedTypeToIcon refinedType =
    Ui.Component.icon <|
        case refinedType of
            Image ->
                Ui.Boxicons.bxImageAlt

            FullName ->
                Ui.Boxicons.bxSmile

            EmailAddress ->
                Ui.Boxicons.bxAt

            Text _ ->
                Ui.Boxicons.bxText

            Int ->
                Ui.Boxicons.bxText

            Float ->
                Ui.Boxicons.bxText

            Bool ->
                Ui.Boxicons.bxText


kindToIcon : Kind -> Element.Element msg
kindToIcon kind =
    case kind of
        List _ ->
            Ui.Component.icon <| Ui.Boxicons.bxListUl

        Single (Just refinedType) ->
            refinedTypeToIcon refinedType

        Single Nothing ->
            Ui.Component.icon <| Ui.Boxicons.bxQuestionMark


compileInterpolationSettings : List String -> TemplateRefinements -> TemplateRefinements
compileInterpolationSettings foundVariables (TemplateRefinements interpolationTypeSettings) =
    let
        -- all found variables minus the ones that were already defined
        notSetVariables =
            foundVariables
                |> List.filter
                    (\name ->
                        case Dict.Extra.find (\_ { variableName } -> name == variableName) interpolationTypeSettings of
                            Nothing ->
                                True

                            Just _ ->
                                False
                    )

        -- for every found variable, find the right setting and use it, else default to Nothing
        interpolationTypeSettingsWithOthers =
            notSetVariables
                |> List.foldl
                    (\var settings_ -> IntDict.insertNew { variableName = var, kind = Text emptyTemplateRefinements } settings_)
                    (interpolationTypeSettings |> Dict.map (\_ { variableName, kind } -> { variableName = variableName, kind = kind }))
                |> Dict.filter (\_ { variableName } -> List.member variableName foundVariables)
    in
    interpolationTypeSettingsWithOthers
        |> TemplateRefinements


setInTemplate (InterpolationVariableKey int) value (TemplateRefinements dict) =
    Dict.insert int value dict
        |> TemplateRefinements


addToTemplate value (TemplateRefinements dict) =
    IntDict.insertNew value dict
        |> TemplateRefinements


interpolationSettingsFromList : List ( InterpolationVariableKey, InterpolationVariableSettings ) -> TemplateRefinements
interpolationSettingsFromList =
    List.map (\( InterpolationVariableKey key, setting ) -> ( key, setting ))
        >> Dict.fromList
        >> TemplateRefinements


interpolationSettingsToList (TemplateRefinements dict) =
    Dict.toList dict
        |> List.map (Tuple.mapFirst InterpolationVariableKey)


mapInterpolationTypeSettings fn (TemplateRefinements dict) =
    Dict.map fn dict
        |> TemplateRefinements


emptyTemplateRefinements =
    Dict.empty
        |> TemplateRefinements


getVariable : InterpolationVariableKey -> TemplateRefinements -> Maybe InterpolationVariableSettings
getVariable (InterpolationVariableKey k) (TemplateRefinements d) =
    Dict.get k d



-- [generator-start]


type alias SelectedEntrySettings =
    { name : String
    , kind : Kind
    }


{-| Either the refined type of a single value or the subselection in a list
-}
type Kind
    = Single (Maybe RefinedType)
    | List Selection


type RefinedType
    = Image
    | EmailAddress
    | FullName
    | Text TemplateRefinements
    | Int
    | Float
    | Bool


type TemplateRefinements
    = TemplateRefinements (Dict.Dict InterpolationVariableKeyRaw InterpolationVariableSettings)


type alias InterpolationVariableSettings =
    { variableName : String
    , kind : RefinedType
    }


type alias InterpolationVariableKeyRaw =
    Int


type InterpolationVariableKey
    = InterpolationVariableKey Int


type alias SelectionEntry =
    ( Node, SelectedEntrySettings )



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeDictInterpolationVariableKeyRawInterpolationVariableSettings =
    let
        decodeDictInterpolationVariableKeyRawInterpolationVariableSettingsTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" decodeInterpolationVariableKeyRaw)
                (Decode.field "A2" decodeInterpolationVariableSettings)
    in
    Decode.map Dict.fromList (Decode.list decodeDictInterpolationVariableKeyRawInterpolationVariableSettingsTuple)


decodeInterpolationVariableKey =
    Decode.map InterpolationVariableKey Decode.int


decodeInterpolationVariableKeyRaw =
    Decode.int


decodeInterpolationVariableSettings =
    Decode.lazy
        (\_ ->
            Decode.map2
                InterpolationVariableSettings
                (Decode.field "variableName" Decode.string)
                (Decode.field "kind" decodeRefinedType)
        )


decodeKind =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeKindHelp


decodeKindHelp constructor =
    case constructor of
        "Single" ->
            Decode.map
                Single
                (Decode.field "A1" (Decode.maybe decodeRefinedType))

        "List" ->
            Decode.map
                List
                (Decode.field "A1" decodeSelection)

        other ->
            Decode.fail <| "Unknown constructor for type Kind: " ++ other


decodeRefinedType =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeRefinedTypeHelp


decodeRefinedTypeHelp constructor =
    case constructor of
        "Image" ->
            Decode.succeed Image

        "EmailAddress" ->
            Decode.succeed EmailAddress

        "FullName" ->
            Decode.succeed FullName

        "Text" ->
            Decode.map
                Text
                (Decode.field "A1" decodeTemplateRefinements)

        "Int" ->
            Decode.succeed Int

        "Float" ->
            Decode.succeed Float

        "Bool" ->
            Decode.succeed Bool

        other ->
            Decode.fail <| "Unknown constructor for type RefinedType: " ++ other


decodeSelectedEntrySettings =
    Decode.map2
        SelectedEntrySettings
        (Decode.field "name" Decode.string)
        (Decode.field "kind" decodeKind)


decodeSelectionEntry =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodeNode)
        (Decode.field "A2" decodeSelectedEntrySettings)


decodeTemplateRefinements =
    Decode.map TemplateRefinements decodeDictInterpolationVariableKeyRawInterpolationVariableSettings


encodeDictInterpolationVariableKeyRawInterpolationVariableSettings a =
    let
        encodeDictInterpolationVariableKeyRawInterpolationVariableSettingsTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", encodeInterpolationVariableKeyRaw a1 )
                , ( "A2", encodeInterpolationVariableSettings a2 )
                ]
    in
    Encode.list encodeDictInterpolationVariableKeyRawInterpolationVariableSettingsTuple (Dict.toList a)


encodeInterpolationVariableKey (InterpolationVariableKey a1) =
    Encode.int a1


encodeInterpolationVariableKeyRaw a =
    Encode.int a


encodeInterpolationVariableSettings a =
    Encode.object
        [ ( "variableName", Encode.string a.variableName )
        , ( "kind", encodeRefinedType a.kind )
        ]


encodeKind a =
    case a of
        Single a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Single" )
                , ( "A1", encodeMaybeRefinedType a1 )
                ]

        List a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "List" )
                , ( "A1", encodeSelection a1 )
                ]


encodeMaybeRefinedType a =
    case a of
        Just b ->
            encodeRefinedType b

        Nothing ->
            Encode.null


encodeRefinedType a =
    case a of
        Image ->
            Encode.object
                [ ( "Constructor", Encode.string "Image" )
                ]

        EmailAddress ->
            Encode.object
                [ ( "Constructor", Encode.string "EmailAddress" )
                ]

        FullName ->
            Encode.object
                [ ( "Constructor", Encode.string "FullName" )
                ]

        Text a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Text" )
                , ( "A1", encodeTemplateRefinements a1 )
                ]

        Int ->
            Encode.object
                [ ( "Constructor", Encode.string "Int" )
                ]

        Float ->
            Encode.object
                [ ( "Constructor", Encode.string "Float" )
                ]

        Bool ->
            Encode.object
                [ ( "Constructor", Encode.string "Bool" )
                ]


encodeSelectedEntrySettings a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "kind", encodeKind a.kind )
        ]


encodeSelectionEntry ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodeNode a1 )
        , ( "A2", encodeSelectedEntrySettings a2 )
        ]


encodeTemplateRefinements (TemplateRefinements a1) =
    encodeDictInterpolationVariableKeyRawInterpolationVariableSettings a1



-- [generator-end]


encodeNode =
    Interface.JsonTree.Model.encodeKeyPath


decodeNode =
    Interface.JsonTree.Model.decodeKeyPath


encodeSelection =
    Dict.Any.toList
        >> Encode.list encodeSelectionEntry


decodeSelection =
    Decode.list decodeSelectionEntry
        |> Decode.map (Dict.Any.fromList Interface.JsonTree.Model.keyPathToString)
