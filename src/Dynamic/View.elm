module Dynamic.View exposing (..)

import Atomic.Helpers
import Atomic.Types
import Dict exposing (Dict)
import Dynamic.Types
import Ordered
import Pathed
import Screen
import Utils


init : Int -> Screen.Screen -> Dynamic.Types.Model
init viewId viewSpec =
    Dict.empty


append : Int -> Atomic.Types.Particle -> Screen.Screen -> Screen.Screen
append key particle viewSpec =
    let
        newElement =
            Atomic.Helpers.mapOrder (always <| Dict.size viewSpec.particles) particle

        view_ =
            { viewSpec | particles = Dict.insert key newElement viewSpec.particles }
    in
    view_


updateName : String -> { a | name : String } -> { a | name : String }
updateName name thing =
    { thing | name = name }


updateClass : String -> { a | class : String } -> { a | class : String }
updateClass class atom =
    { atom | class = class }


setOnAtom :
    Int
    -> (Atomic.Types.Atom -> Atomic.Types.Atom)
    -> Screen.Screen
    -> Screen.Screen
setOnAtom id fn =
    mapElements
        (Dict.update id (Maybe.map (mapAtom fn)))


setClass : Int -> String -> Screen.Screen -> Screen.Screen
setClass id class =
    mapElements (Dict.update id (Maybe.map <| Atomic.Helpers.mapClass (always class)))


setNameOnAtom : Int -> String -> Screen.Screen -> Screen.Screen
setNameOnAtom id name =
    setOnAtom id (updateName name)


removeParticlesInView : List Pathed.Path -> Utils.HomoMorphism Screen.Screen
removeParticlesInView pathsToRemove =
    Utils.pipe (List.map removeParticleInView pathsToRemove)


{-| Traverse a tree rooted by a particle by path and map the particle at the given location through a function.
-}
mapParticle :
    Pathed.Path
    -> Utils.HomoMorphism Atomic.Types.Particle
    -> Utils.HomoMorphism Screen.Screen
mapParticle path fn screen =
    let
        map key fn_ =
            Dict.update key (Maybe.map fn_)

        screen_ =
            { screen | particles = descent path screen.particles }

        descent : Pathed.Path -> Utils.HomoMorphism (Dict.Dict Int Atomic.Types.Particle)
        descent { id, ancestry } =
            case ancestry of
                ancestor :: rest_ ->
                    map ancestor (Atomic.Helpers.mapParticles (descent (Pathed.Path rest_ id)))

                [] ->
                    map id fn
    in
    screen_


{-| Traverse the tree by path and map the contained in a group through a function.
-}
mapChildrenOfParticleInScreen :
    Pathed.Path
    -> Utils.HomoMorphism (Dict Int Atomic.Types.Particle)
    -> Utils.HomoMorphism Screen.Screen
mapChildrenOfParticleInScreen path fn screen =
    let
        map key fn_ =
            Dict.update key (Maybe.map fn_)

        descent : Pathed.Path -> Utils.HomoMorphism (Dict.Dict Int Atomic.Types.Particle)
        descent { id, ancestry } =
            case ancestry of
                ancestor :: rest ->
                    map ancestor (Atomic.Helpers.mapParticles (descent (Pathed.Path rest id)))

                [] ->
                    map id (Atomic.Helpers.mapParticles fn)
    in
    { screen
        | particles = descent path screen.particles
    }


{-| Given a location in a tree insert a particle also with regard to the order of the particles
-}
addParticleAtLocationInScreen : Screen.DropTarget -> Int -> Atomic.Types.Particle -> Utils.HomoMorphism Screen.Screen
addParticleAtLocationInScreen ( ordinalRelation, path ) particleId particle screen =
    let
        insertNextToSibling : ( Screen.OrdinalRelation, Int ) -> Atomic.Types.Particle -> Utils.HomoMorphism (Dict Int Atomic.Types.Particle)
        insertNextToSibling ( ordinalRelation_, siblingId ) particle_ particles =
            let
                newIndex : Maybe Int
                newIndex =
                    Dict.get siblingId particles
                        |> Maybe.map Atomic.Helpers.getOrder
                        |> Maybe.map
                            (\o ->
                                case ordinalRelation_ of
                                    Screen.Before ->
                                        o - 1

                                    -- only necessary when particle has to be put as first element in list
                                    Screen.After ->
                                        o
                            )
            in
            case newIndex of
                Just afterIndex ->
                    Dict.insert particleId particle particles
                        |> Ordered.reorder particleId afterIndex

                Nothing ->
                    particles

        res =
            mapChildrenOfParticleInScreen
                path
                (insertNextToSibling ( ordinalRelation, path.id ) particle)
                screen
    in
    res


{-| Traverse the tree by path and map the particle at the given location through a function.
-}
getParticleInScreen :
    Pathed.Path
    -> Screen.Screen
    -> Maybe Atomic.Types.Particle
getParticleInScreen path screen =
    let
        descent : Pathed.Path -> Dict.Dict Int Atomic.Types.Particle -> Maybe Atomic.Types.Particle
        descent { ancestry, id } particles =
            case ancestry of
                [] ->
                    Dict.get id particles

                ancestor :: rest ->
                    Dict.get ancestor particles
                        |> Maybe.map Atomic.Helpers.getParticles
                        |> Maybe.andThen (descent (Pathed.Path rest id))
    in
    descent path screen.particles


{-| Traverse the tree by path and remove the element at the given location.
-}
removeParticleInView :
    Pathed.Path
    -> Utils.HomoMorphism Screen.Screen
removeParticleInView path screen =
    let
        map key fn_ =
            Dict.update key (Maybe.map fn_)

        screen_ =
            { screen | particles = descent path screen.particles }

        descent : Pathed.Path -> Utils.HomoMorphism (Dict.Dict Int Atomic.Types.Particle)
        descent { id, ancestry } =
            case ancestry of
                [] ->
                    Dict.filter (\particleKey _ -> id /= particleKey)

                -- remove particles with matching ancestors
                ancestor :: rest ->
                    map ancestor (Atomic.Helpers.mapParticles (descent (Pathed.Path rest id)))
    in
    screen_


mapElements :
    (Dict Int Atomic.Types.Particle -> Dict Int Atomic.Types.Particle)
    -> Screen.Screen
    -> Screen.Screen
mapElements updater viewSpec =
    { viewSpec | particles = updater viewSpec.particles }


setOnOrganism :
    Int
    -> (Atomic.Types.Organism -> Atomic.Types.Organism)
    -> Screen.Screen
    -> Screen.Screen
setOnOrganism id fn =
    mapElements
        (Dict.update id (Maybe.map (mapOrganism fn)))


mapOrganism :
    (Atomic.Types.Organism -> Atomic.Types.Organism)
    -> Atomic.Types.Particle
    -> Atomic.Types.Particle
mapOrganism fn p =
    case p of
        Atomic.Types.SomeOrganism o ->
            Atomic.Types.SomeOrganism (fn o)

        _ ->
            p


mapAtom :
    (Atomic.Types.Atom -> Atomic.Types.Atom)
    -> Atomic.Types.Particle
    -> Atomic.Types.Particle
mapAtom fn p =
    case p of
        Atomic.Types.SomeAtom a ->
            Atomic.Types.SomeAtom (fn a)

        _ ->
            p
