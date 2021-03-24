module Spec exposing (..)

{-| The spec describes an entire funk application that a user can work on

The idea is to compile a funk spec to an elm codebase (but that is for the future).

-}

import Canvas.Events
import Interface
import Interface.Data
import Interface.Model
import Json.Decode as Decode
import List.Extra
import Rectangle
import Set
import Slug
import Spec.DataConnection
import Spec.Element
import Spec.Element.Id
import Spec.Element.Layout.Length
import Spec.Element.Model
import Spec.Element.Style
import Spec.Model



-- typ annotation breaks https://github.com/elm/compiler/issues/2083


hasTextConnection : Spec.Element.Model.EitherElement -> Spec.Model.WithSpec a -> Bool
hasTextConnection element userModel =
    let
        dataConnection =
            Spec.Element.Id.getFromDict
                element.shared.id
                userModel.dataConnections
    in
    case ( element.outerGeometry, dataConnection ) of
        -- screens have no text connection ever
        ( Spec.Element.Model.ScreenGeometry _, _ ) ->
            False

        ( _, Just (Spec.DataConnection.Static (Interface.Data.ParagraphText _)) ) ->
            True

        _ ->
            False


getStyle : Spec.Element.Id.Id -> Spec.Model.WithSpec a -> Spec.Element.Style.Style
getStyle id { elementStyles } =
    Spec.Element.Id.getFromDict id elementStyles
        |> Maybe.withDefault (Spec.Element.Style.default Spec.Element.Model.Box)


{-| retrieve the dimensions of a rectangle

TODO: enhance with result type to give user feedback when

  - his input does not parse
  - the connected interface pointer was not found

-}
getAbsoluteElementRectangle :
    Spec.Model.WithSpec a
    -> Interface.Model.ScopeData
    -> Spec.Element.Model.AbsoluteElementDimensions
    -> Canvas.Events.ElementRectangle
getAbsoluteElementRectangle userModel scope { x, y, width, height } =
    let
        dimensions : Rectangle.Dimensions
        dimensions =
            { x = resolve userModel scope x
            , y = resolve userModel scope y
            , width = resolve userModel scope width
            , height = resolve userModel scope height
            }
    in
    Rectangle.fromDimensionsFloat dimensions
        |> Canvas.Events.ElementRectangle


resolve : Spec.Model.WithSpec userModel -> Interface.Model.ScopeData -> Spec.Element.Layout.Length.Length -> Float
resolve userModel scope value =
    case value.current of
        Spec.Element.Layout.Length.UserInput _ ->
            value.last

        Spec.Element.Layout.Length.Linked interfacePointer ->
            let
                rawValue =
                    Interface.retrieveValue
                        { interfacePointer = interfacePointer
                        , model = userModel
                        , scope = scope
                        }
            in
            rawValue
                |> Maybe.andThen (Decode.decodeValue Decode.float >> Result.toMaybe)
                |> Maybe.withDefault (value.last |> round |> toFloat)


resolveNullable : Spec.Model.WithSpec userModel -> Interface.Model.ScopeData -> Spec.Element.Layout.Length.NullableLength -> Maybe Float
resolveNullable userModel scope value =
    case value.current of
        Spec.Element.Layout.Length.UserInput _ ->
            value.last

        Spec.Element.Layout.Length.Linked interfacePointer ->
            let
                rawValue =
                    Interface.retrieveValue
                        { interfacePointer = interfacePointer
                        , model = userModel
                        , scope = scope
                        }

                decodedValue =
                    rawValue
                        |> Maybe.andThen (Decode.decodeValue Decode.float >> Result.toMaybe)
            in
            case decodedValue of
                Nothing ->
                    value.last |> Maybe.map (round >> toFloat)

                Just val ->
                    Just val


getStringValue : Spec.Model.WithSpec userModel -> Interface.Model.ScopeData -> Spec.Element.Layout.Length.Length -> String
getStringValue userModel scope value =
    case value.current of
        Spec.Element.Layout.Length.UserInput str ->
            str

        Spec.Element.Layout.Length.Linked interfacePointer ->
            Interface.retrieveValue { interfacePointer = interfacePointer, model = userModel, scope = scope }
                |> Maybe.andThen (Decode.decodeValue Decode.float >> Result.toMaybe)
                |> Maybe.withDefault value.last
                |> String.fromFloat


getStringValueNullable : Spec.Model.WithSpec userModel -> Interface.Model.ScopeData -> Spec.Element.Layout.Length.NullableLength -> String
getStringValueNullable userModel scope value =
    case value.current of
        Spec.Element.Layout.Length.UserInput str ->
            str

        Spec.Element.Layout.Length.Linked interfacePointer ->
            Interface.retrieveValue { interfacePointer = interfacePointer, model = userModel, scope = scope }
                |> Maybe.andThen (Decode.decodeValue Decode.float >> Result.toMaybe)
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault "- (link not found)"


{-| Use the user-given names of a screen to generate a URL slug
-}
getScreensWithUniqueSlugs : Spec.Model.WithSpec userModel -> List ( Slug.Slug, Spec.Element.Model.Screen )
getScreensWithUniqueSlugs { itemsOnCanvas } =
    let
        init : ( List ( Slug.Slug, Spec.Element.Model.Screen ), Set.Set String )
        init =
            ( [], Set.empty )

        ( result, _ ) =
            List.Extra.indexedFoldl
                slugReducer
                init
                itemsOnCanvas
    in
    result


slugReducer :
    Int
    -> Spec.Element.Model.Screen
    -> ( List ( Slug.Slug, Spec.Element.Model.Screen ), Set.Set String )
    -> ( List ( Slug.Slug, Spec.Element.Model.Screen ), Set.Set String )
slugReducer index screen ( screens, foundNames ) =
    let
        result =
            case screen.shared.label of
                "" ->
                    case Slug.generate (String.fromInt index) of
                        Just slug ->
                            Just ( ( slug, screen ), foundNames )

                        Nothing ->
                            Nothing

                name ->
                    case Set.member name foundNames of
                        True ->
                            -- when the name was found before, add a number suffix
                            case Slug.generate (name ++ "-" ++ String.fromInt index) of
                                Nothing ->
                                    Nothing

                                Just slug ->
                                    Just ( ( slug, screen ), foundNames )

                        False ->
                            -- when the name is unique, do not use a number suffix
                            case Slug.generate name of
                                Nothing ->
                                    Nothing

                                Just slug ->
                                    Just ( ( slug, screen ), Set.insert name foundNames )

        -- we need to filtermap here because slug may be nothign
    in
    case result of
        Just ( newScreen, newFoundNames ) ->
            ( screens ++ [ newScreen ], newFoundNames )

        Nothing ->
            ( screens, foundNames )
