module Rectangle.Tree exposing (..)

import Canopy
import List.Extra
import Rectangle
import Spec.Element.Id
import Spec.Model


{-| The tree index holds trees of rectangles
entries are sorted descending by size of the rectangle in root
Each root has children that are directly enclosed in the root
This means that three nested rectangles (visualizing the drawing)
`A [ B [ C ]]`
are first measured and described as two shallow trees
`(A, [B]) and (B, [C])`
From this, we can then build a deep tree by
replacing leaves on the root with a shallow tree whiches root has the same ID
`A - B - C`
-}
type alias TreeIndex =
    { root : RectIndexEntry
    , rest : RectIndex
    }


{-| A list of shallow, i.e. 1-level-deep trees.
-}
type alias RectIndex =
    List RectIndexEntry


type alias RectIndexEntry =
    { node : RectWithId, children : List RectWithId }


type alias RectWithId =
    ( Spec.Element.Id.Id, Rectangle.Rectangle )


buildTree : TreeIndex -> Canopy.Node RectWithId
buildTree { root, rest } =
    let
        rootNode =
            Canopy.node root.node
    in
    rootNode <| buildChildren root.children rest


{-| Build a tree from the rectangle index with parent-child relation
-- --> (A, [B]) and (B, [C])
-}
buildChildren : List RectWithId -> RectIndex -> List (Canopy.Node RectWithId)
buildChildren rootChildren rectIndex =
    List.foldl
        (\nextChild childrenSoFar ->
            let
                ( nextChildId, nextChildRect ) =
                    nextChild

                -- prepare the next child of our root node
                child =
                    Canopy.node nextChild
            in
            case List.Extra.find (.node >> sameId nextChild) rectIndex of
                -- if this child itself has no children, just add it to our root's childlist
                Nothing ->
                    childrenSoFar ++ [ child [] ]

                -- otherwise go deeper and build this child's children's children
                Just { children } ->
                    childrenSoFar ++ [ child (buildChildren children rectIndex) ]
        )
        []
        rootChildren


type TreeBuildingError
    = CanNotContain RectWithId
    | NoUniqueRoot RectWithId
    | Empty


type alias TreeIndexResult =
    Result TreeBuildingError TreeIndex


type alias TreeResult =
    Result TreeBuildingError Tree


type alias Tree =
    Canopy.Node RectWithId


{-| Build a tree based on rectangles containing each other
assume we have four rectangles
where rect 0 is the biggest rectangle and it Rectangle.contains all other rectangles
we take the next smallest rect 1 and check if it fits into rect 1 -- REDUNDANT WITH PRESUMPTION
if ok then add this to the child list
if nok then produce an error

now that we have the root with one element, we go to the next element
next rect is rect 2
check if it fits into rect 1, put it as a child there
if not, check if it fits into rect 0 and put it as a child there
else produce an error

[1, 2] 3

-}
toNodeRegistry : Spec.Model.Rectangles -> Result TreeBuildingError RectIndex
toNodeRegistry rects =
    case sortRectsByDescendingSize rects of
        [] ->
            Err Empty

        root :: rest ->
            let
                registryWithRootOnly =
                    [ RectIndexEntry root [] ]
            in
            associateRectsByContainment registryWithRootOnly rest
                |> Result.map List.reverse


sortRectsByDescendingSize : Spec.Model.Rectangles -> List RectWithId
sortRectsByDescendingSize =
    Spec.Element.Id.dictToList
        >> List.sortBy (Tuple.second >> Rectangle.size)
        >> List.reverse


{-| Build a tree index, that is a list of shallow trees based on rectangle geometries
Each rectangle will result in a root for a tree.
We then try attaching a each rectangle as a leaf to the next bigger rectangle.
Because we Are looking at the next closest rectangle by size we ensure that we traverse our
rectangles in nested order as rectangles can only be contained if they are smaller than their closest parent by size.
-}
associateRectsByContainment : RectIndex -> List RectWithId -> Result TreeBuildingError RectIndex
associateRectsByContainment registrySoFar drawn =
    case drawn of
        -- no children below root there are young jedi
        [] ->
            Ok registrySoFar

        -- there are children below root
        nextRect :: otherRects ->
            case findNextBestParent registrySoFar nextRect of
                Ok updatedEntry ->
                    let
                        registryWithThisRectAssociatedToItsParent =
                            List.Extra.setIf
                                (\someEntry -> sameId someEntry.node updatedEntry.node)
                                updatedEntry
                                registrySoFar

                        registryWithThisRectAsParent =
                            RectIndexEntry nextRect [] :: registryWithThisRectAssociatedToItsParent

                        final =
                            registryWithThisRectAsParent
                    in
                    associateRectsByContainment final otherRects

                Err err ->
                    Err err


{-| Find the actual parent in an existing index where all bigger rectangles are held.
-}
findNextBestParent : RectIndex -> RectWithId -> Result TreeBuildingError RectIndexEntry
findNextBestParent registry rect =
    case registry of
        -- try fitting into next bigger rectangle
        nextBiggerEntry :: evenBiggerEntries ->
            let
                fitsIntoNextBiggerRectangle =
                    Rectangle.contains
                        (Tuple.second nextBiggerEntry.node)
                        (Tuple.second rect)
            in
            if fitsIntoNextBiggerRectangle then
                Ok { nextBiggerEntry | children = nextBiggerEntry.children ++ [ rect ] }
                -- if we can not, look at even bigger rectangles

            else
                findNextBestParent evenBiggerEntries rect

        -- If we run out of rectangles that might contain our subject we throw
        [] ->
            Err (CanNotContain rect)


{-| Given a dictionary of rectangles, sort them and try building a tree that reflects containment
-}
fromRects : Spec.Model.Rectangles -> Result TreeBuildingError (Canopy.Node RectWithId)
fromRects rects =
    let
        registry =
            toNodeRegistry rects

        treeResult =
            case registry of
                Ok (rootWithChildren :: otherNodesWithChildren) ->
                    Ok <| buildTree { root = rootWithChildren, rest = otherNodesWithChildren }

                Err err ->
                    Err err

                _ ->
                    Err Empty
    in
    treeResult



-- HELPERS


errToSting err =
    case err of
        CanNotContain _ ->
            "No page boundary found. Please draw a page around all elements"

        NoUniqueRoot _ ->
            "No page boundary found. Please draw a page around all elements"

        Empty ->
            "Empty"


id : ( Spec.Element.Id.Id, a ) -> Spec.Element.Id.Id
id =
    Tuple.first


sameId : ( Spec.Element.Id.Id, a ) -> ( Spec.Element.Id.Id, b ) -> Bool
sameId a b =
    id a == id b
