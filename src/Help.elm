module Help exposing (..)

{-| Various helpers
-}

import Dict
import List.Extra
import String.Extra


type alias HomoMorphism a =
    a -> a


{-| Chain a list of functions
-}
pipe : List (HomoMorphism a) -> HomoMorphism a
pipe fns arg =
    List.foldl
        (\update_ model_ -> update_ model_)
        arg
        fns


flatten2D : List (List a) -> List a
flatten2D list =
    List.foldr (++) [] list


{-| Turn a list of tuples into a dict with identical key/values
This is useful for generating a generic mapping of particles on themselves
-}
isoDict : List ( Int, a ) -> Dict.Dict Int Int
isoDict idList =
    List.map (\( id, _ ) -> ( id, id )) idList
        |> Dict.fromList


toggleInList : a -> List a -> List a
toggleInList item list =
    if List.member item list then
        List.Extra.remove item list

    else
        item :: list


{-| Turn any string into something that can be parsed as a JS variable

    toJsVariableName "hello world" --> "helloWorld"

-}
toJsVariableName : String -> String
toJsVariableName =
    String.Extra.humanize >> String.Extra.classify >> String.Extra.decapitalize
