module Interpolator exposing (..)

import Dict
import Mustache
import Regex


{-| Replace a single variable in a mustach template

    interpolateOne "planet" "earth" "welcome to planet {{planet}} of {{planets}}"
    --> "welcome to planet earth of {{planets}}"

-}
interpolateOne : String -> String -> String -> String
interpolateOne variableName stringToInject template =
    Regex.replace Mustache.variableRegex (processMarker variableName stringToInject) template


processMarker : String -> String -> Regex.Match -> String
processMarker variableName stringToInject match =
    let
        submatchName =
            List.head match.submatches
                |> Maybe.andThen identity

        isVariableToReplace =
            submatchName == Just variableName
    in
    if isVariableToReplace then
        stringToInject

    else
        match.match


{-| Find interpolatable variables in string

    getVariables "{{hello}} pretty {{world}}" --> ["hello", "world"]

-}
getVariables : String -> List String
getVariables template =
    let
        getMatchVariableName =
            .match >> String.dropLeft 2 >> String.dropRight 2 >> String.trim

        matches =
            Regex.find Mustache.variableRegex template
                |> List.map getMatchVariableName
    in
    matches


{-| Render interpolatable variables in string

    import Dict

    render (Dict.fromList [("greeting", "hi"), ("subject", "sunset")] )"{{greeting}} pretty {{subject}}"
    --> "hi pretty sunset"

-}
render : Dict.Dict String String -> String -> String
render vars template =
    let
        nodes =
            Dict.toList vars
                |> List.map (\( key, val ) -> Mustache.Variable key val)
    in
    Mustache.render nodes template
