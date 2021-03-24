module CodeEditor.Help exposing (..)

import ApiExplorer
import ApiExplorer.Model
import CodeEditor.Model
import Help
import Json.Encode as Encode
import Model.Model


getArgs :
    Model.Model.UserModel
    -> CodeEditor.Model.Transformation
    ->
        { parameterNames : List String
        , parameterValues : List Encode.Value
        }
getArgs userModel transformation =
    let
        -- for every selected api
        -- get the api data
        -- stuff them into an object
        -- and turn into an argument
        ( names, args ) =
            transformation.arguments
                |> List.filterMap
                    (\key ->
                        case ApiExplorer.Model.getApiSpec key userModel.apiExplorer of
                            Nothing ->
                                Nothing

                            Just spec ->
                                case ApiExplorer.getAsJson spec userModel.apiExplorer of
                                    Nothing ->
                                        Nothing

                                    Just data ->
                                        Just
                                            ( spec.name |> Help.toJsVariableName
                                            , data
                                            )
                    )
                |> List.unzip
    in
    { parameterNames = names, parameterValues = args }


paramsStartDelimiter =
    "/*args-start*/"


paramsEndDelimiter =
    "/*args-end*/"


injectParameters parameterNames transformation =
    case String.split paramsStartDelimiter transformation.code of
        [ beforeParameters, tail ] ->
            case String.split paramsEndDelimiter tail of
                [ parameters, afterParameters ] ->
                    let
                        newParams =
                            String.join ", " parameterNames
                    in
                    beforeParameters
                        ++ paramsStartDelimiter
                        ++ " "
                        ++ newParams
                        ++ " "
                        ++ paramsEndDelimiter
                        ++ afterParameters

                _ ->
                    transformation.code

        _ ->
            transformation.code
