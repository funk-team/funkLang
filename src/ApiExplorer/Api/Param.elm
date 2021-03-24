module ApiExplorer.Api.Param exposing (simplify, validate, validateAll)

import ApiExplorer.Api
import Dynamic.Data
import Interface.Selection
import Regex
import Result.Extra


simplify : ApiExplorer.Api.ApiParam -> { name : String, refinedType : Interface.Selection.RefinedType }
simplify { variable, refinedType, fieldName } =
    case variable of
        Just variableSettings ->
            { name = fieldName ++ " {{" ++ variableSettings.name ++ "}}", refinedType = variableSettings.refinedType }

        Nothing ->
            { name = fieldName, refinedType = refinedType }


{-| <https://regexr.com/39c7p>
-- TODO: accepts names with at least 5 characters
-}
validateFullName : String -> Result String ()
validateFullName someText =
    let
        error =
            Err "Please enter your full name"
    in
    case String.split " " (String.trim someText) of
        first :: last :: _ ->
            let
                atLeastTwoLetters word =
                    String.length word >= 2
            in
            if atLeastTwoLetters first && atLeastTwoLetters last then
                Ok ()

            else
                error

        _ ->
            error


{-| <https://emailregex.com/>
-}
validateEmail : String -> Result String ()
validateEmail someText =
    case Regex.fromString "^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$" of
        Nothing ->
            Err "Email validation is broken. Please tell jan@funklang.com"

        Just re ->
            case Regex.contains re someText of
                True ->
                    Ok ()

                False ->
                    Err "Please enter a valid E-Mail address"


validate : ApiExplorer.Api.ApiParam -> Result String ()
validate { data, refinedType, variable } =
    let
        kind =
            case variable of
                Just v ->
                    v.refinedType

                Nothing ->
                    refinedType
    in
    case ( data, kind ) of
        ( Dynamic.Data.StringInstance str, Interface.Selection.EmailAddress ) ->
            validateEmail (String.trim str)

        ( Dynamic.Data.StringInstance str, Interface.Selection.FullName ) ->
            validateFullName (String.trim str)

        _ ->
            Ok ()


validateAll : List ApiExplorer.Api.ApiParam -> Bool
validateAll =
    List.map validate
        >> List.all Result.Extra.isOk
