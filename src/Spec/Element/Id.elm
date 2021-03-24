module Spec.Element.Id exposing (..)

import Dict
import Element
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Random.Int
import Set
import Util


random : Random.Seed -> ( Id, Random.Seed )
random =
    Random.step Random.Int.positiveInt
        >> Tuple.mapFirst idFromInt


type Set
    = Set (Set.Set Int)


type Dict a
    = Dict (Dict.Dict Int a)


toHtmlId : Id -> Element.Attribute msg
toHtmlId id =
    Element.htmlAttribute (Html.Attributes.id (toHtmlIdRaw id))


toDeclarationName (Id id) =
    "el" ++ String.fromInt id


{-| Given an element id render the content of the HTML id attribute
-}
toHtmlIdRaw : Id -> String
toHtmlIdRaw (Id id) =
    String.fromInt id |> (++) prefix


prefix =
    "el-"


parseFromHtmlId : String -> Maybe Id
parseFromHtmlId str =
    if String.startsWith prefix str then
        str
            |> String.dropLeft (String.length prefix)
            |> String.toInt
            |> Maybe.map idFromInt

    else
        Nothing


toString (Id id) =
    String.fromInt id


emptySet =
    Set Set.empty


singletonSet (Id id) =
    Set.singleton id |> Set


removeFromDict (Id id) (Dict dict) =
    Dict <| Dict.remove id dict


insertIntoDict (Id id) el (Dict dict) =
    Dict <| Dict.insert id el dict


insertIntoSet (Id id) (Set dict) =
    Set <| Set.insert id dict


isEmptyDict (Dict dict) =
    Dict.isEmpty dict


isEmptySet (Set dict) =
    Set.isEmpty dict


dictToList : Dict a -> List ( Id, a )
dictToList (Dict dict) =
    Dict.toList dict
        |> List.map (Tuple.mapFirst Id)


dictFromList : List ( Id, a ) -> Dict a
dictFromList =
    List.map (Tuple.mapFirst (\(Id id) -> id))
        >> Dict.fromList
        >> Dict


dictKeys : Dict a -> List Id
dictKeys (Dict dict) =
    Dict.keys dict
        |> List.map idFromInt


dictUnion : Dict a -> Dict a -> Dict a
dictUnion (Dict dictA) (Dict dictB) =
    Dict.union dictA dictB
        |> Dict


dictIntersect : Dict a -> Dict a -> Dict a
dictIntersect (Dict dictA) (Dict dictB) =
    Dict.intersect dictA dictB
        |> Dict


getFromDict (Id id) (Dict dict) =
    Dict.get id dict


mapDict fn (Dict dict) =
    Dict.map fn dict
        |> Dict


updateDict : Id -> (Maybe a -> Maybe a) -> Dict a -> Dict a
updateDict (Id id) fn (Dict dict) =
    Dict.update id fn dict
        |> Dict


elementIdSetToList : Set -> List Id
elementIdSetToList (Set set) =
    Set.toList set
        |> List.map Id


elementIdSetFromList : List Id -> Set
elementIdSetFromList =
    List.map (\(Id id) -> id)
        >> Set.fromList
        >> Set


elementIdSetMember : Id -> Set -> Bool
elementIdSetMember (Id id) (Set set) =
    Set.member id set


dictMember : Id -> Dict a -> Bool
dictMember (Id id) (Dict set) =
    Dict.member id set


type alias RootId =
    String


htmlRootId =
    Html.Attributes.id rootId
        |> Element.htmlAttribute


rootId : RootId
rootId =
    "canvas-root"


rootIdInt =
    0


toggleInSet : Id -> Set -> Set
toggleInSet (Id key) (Set set) =
    if Set.member key set then
        Set <| Set.remove key set

    else
        Set <| Set.insert key set


idFromInt : Int -> Id
idFromInt =
    Id



-- TODO - fix these in elm-coder-generator


encodeSet (Set set) =
    Set.toList set
        |> Encode.list Encode.int


decodeSet =
    Decode.list Decode.int
        |> Decode.map (Set.fromList >> Set)


emptyDict =
    Dict Dict.empty


encodeDict a (Dict dict) =
    encodeDictInta a dict


decodeDict a =
    decodeDictInta a
        |> Decode.map Dict


encodeDictInta encodea a =
    let
        encodeDictIntaTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.int a1 )
                , ( "A2", encodea a2 )
                ]
    in
    Encode.list encodeDictIntaTuple (Dict.toList a)


decodeDictInta decodea =
    let
        decodeDictIntaTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" Decode.int)
                (Decode.field "A2" decodea)
    in
    Decode.map Dict.fromList (Decode.list decodeDictIntaTuple)


removeManyFromDict : Set -> Dict a -> Dict a
removeManyFromDict ids =
    List.map removeFromDict (elementIdSetToList ids)
        |> Util.pipe



-- [decgen-start]


type Id
    = Id Int



-- [decgen-generated-start] -- DO NOT MODIFY or remove this line


decodeId =
    Decode.map Id Decode.int


encodeId (Id a1) =
    Encode.int a1



-- [decgen-end]
