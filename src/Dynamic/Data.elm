module Dynamic.Data exposing (..)

import Color
import Color.Extra
import Dict exposing (Dict)
import Dict.Extra
import Element
import Help
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra


print : Instance -> Element.Element msg
print i =
    case i of
        StringInstance str ->
            Element.text str

        ImageInstance str ->
            Element.text str

        NumberInstance num ->
            Element.text (String.fromFloat num)

        BoolInstance true ->
            Element.text
                (if true then
                    "True"

                 else
                    "False"
                )

        UnionInstance variants ( int, _ ) ->
            Element.text (Dict.get int variants |> Maybe.map Tuple.first |> Maybe.withDefault "[variant not found]")

        _ ->
            Element.text "..."


toKind : Instance -> Kind
toKind i =
    case i of
        StringInstance str ->
            StringKind

        ImageInstance str ->
            ImageKind

        NumberInstance num ->
            NumberKind

        BoolInstance true ->
            BoolKind

        ComplexInstance cpl ->
            cpl
                |> Dict.map
                    (\key { name, instance } ->
                        NamedKind name (toKind instance)
                    )
                |> ComplexKind

        ListInstance dataType_ _ ->
            ListKind dataType_

        GeoPointInstance _ ->
            GeoPointKind

        ColorInstance _ ->
            ColorKind

        NullInstance ->
            NullKind

        NullableInstance type_ _ ->
            NullableKind type_

        UnionInstance type_ _ ->
            UnionKind type_


humanReadableKind t =
    case t of
        BoolKind ->
            "Boolean"

        DateTimeKind ->
            "Timestamp"

        GeoPointKind ->
            "Geolocation Coordinates"

        ColorKind ->
            "Color"

        ImageKind ->
            "Image"

        NumberKind ->
            "Number"

        StringKind ->
            "Text"

        ListKind subtype ->
            humanReadableKind subtype ++ " list"

        ComplexKind _ ->
            "{}"

        NullKind ->
            "Nothing"

        NullableKind subtype ->
            "Maybe a " ++ humanReadableKind subtype

        UnionKind constructors ->
            "Custom Type"


kindDescription t =
    case t of
        BoolKind ->
            "Useful or when things are 'on' or 'off'"

        DateTimeKind ->
            "A day or time"

        GeoPointKind ->
            "A location on planet earth"

        ColorKind ->
            "Red, green, blue, anything you can put in RGBA."

        ImageKind ->
            "A photo, drawing or other artwork"

        NumberKind ->
            "The stuff we used in math class"

        StringKind ->
            "Ramble a bit"

        ListKind subtype ->
            "An list of things"

        ComplexKind _ ->
            "Like a directory structure: File name points to file contents"

        NullKind ->
            "Seriously nothing at all"

        NullableKind subtype ->
            "Could be something or nothign at all"

        UnionKind constructors ->
            "When you have different options but boolean doesn't work because there are more than two."


viewConstructor : ( String, List Kind ) -> String
viewConstructor ( name, subKinds ) =
    let
        subKindsReadable =
            subKinds
                |> List.map humanReadableKind
                |> String.join " and "
    in
    name
        ++ " "
        ++ subKindsReadable


compatible : NamedKind -> NamedKind -> Bool
compatible a b =
    a.type_ == b.type_


compatibleInstance : Instance -> Instance -> Bool
compatibleInstance a b =
    toKind a == toKind b


humanReadableFromNamed : NamedInstance -> ( String, String )
humanReadableFromNamed { name, instance } =
    humanReadable instance


humanReadable : Instance -> ( String, String )
humanReadable d =
    humanReadableWithDict Nothing d


humanReadableWithDict : Maybe Kind -> Instance -> ( String, String )
humanReadableWithDict _ d =
    case d of
        UnionInstance variants i ->
            ( "Type", Dict.get (Tuple.first i) variants |> Maybe.map Tuple.first |> Maybe.withDefault "Whoopsie" )

        -- @@TODO refactor union type to not allow for bad selections
        StringInstance str ->
            ( "Text", "\"" ++ str ++ "\"" )

        ImageInstance str ->
            ( "Image", "🖼" )

        NumberInstance num ->
            ( "Number", String.fromFloat num )

        BoolInstance true ->
            ( "Boolean"
            , if true then
                "True"

              else
                "False"
            )

        ComplexInstance cpl ->
            let
                str =
                    cpl
                        |> Dict.toList
                        |> List.map (\( _, { name, instance } ) -> name ++ ":" ++ (Tuple.second <| humanReadable instance))
                        |> String.join ": "
                        |> (\str_ -> "{" ++ str_ ++ "}")
            in
            ( "Complex", str )

        ListInstance dataType_ entries ->
            let
                first3AndTail =
                    List.Extra.splitAt 3 entries

                remaining =
                    List.length (Tuple.second first3AndTail)

                remainderHint =
                    if remaining > 0 then
                        " +" ++ String.fromInt remaining

                    else
                        ""

                type_ =
                    humanReadableKind dataType_

                readableEntries =
                    first3AndTail
                        |> Tuple.first
                        |> List.map (humanReadableWithDict (Just dataType_) >> Tuple.second)
                        |> String.join ", "
                        |> (\str_ -> " [" ++ str_ ++ remainderHint ++ "]")
            in
            ( "List", type_ ++ readableEntries )

        ColorInstance color ->
            ( "Color", Color.Extra.toCssString color )

        GeoPointInstance { lat, lng } ->
            ( "Location", "lat: " ++ String.fromFloat lat ++ ", lng: " ++ String.fromFloat lng )

        NullInstance ->
            ( "Nothing", "\u{1F937}" )

        NullableInstance type_ val ->
            ( "Maybe one " ++ humanReadableKind type_
            , case val of
                Nothing ->
                    "Nothing"

                Just a ->
                    humanReadable a |> Tuple.second
            )



{-
   Combine type information with an instance to give names to a dictionary
-}


toNamedInstance : Kind -> Dict Int Instance -> Dict Int NamedInstance
toNamedInstance type_ =
    Dict.Extra.filterMap
        (\key val ->
            case type_ of
                ComplexKind namedType ->
                    case Dict.get key namedType of
                        Just { name } ->
                            Just { name = name, instance = val }

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        )


{-| Todo - deduce new type of list
-}
filterMapList :
    (Instance -> Maybe Instance)
    -> Help.HomoMorphism Instance
filterMapList fn l =
    case l of
        ListInstance type_ values ->
            ListInstance type_ (List.filterMap fn values)

        _ ->
            l



-- MAP FUNCTION


mapComplex :
    Help.HomoMorphism (Dict.Dict Int NamedInstance)
    -> Help.HomoMorphism Instance
mapComplex fn instance =
    case instance of
        ComplexInstance cpl ->
            ComplexInstance (fn cpl)

        _ ->
            instance



-- MAP FUNCTION


fromComplex :
    Instance
    -> Maybe (Dict Int NamedInstance)
fromComplex instance =
    case instance of
        ComplexInstance cpl ->
            Just cpl

        _ ->
            Nothing


toBoolInstance :
    Bool
    -> Instance
toBoolInstance =
    BoolInstance


toStringInstance :
    String
    -> Instance
toStringInstance =
    StringInstance



-- MAP FUNCTION


fromComplexType :
    Kind
    -> Maybe (Dict Int NamedKind)
fromComplexType type_ =
    case type_ of
        ComplexKind cpl ->
            Just cpl

        _ ->
            Nothing


extractOptions : Kind -> Maybe (List ( String, Instance ))
extractOptions type_ =
    case type_ of
        BoolKind ->
            [ ( "Yes", True )
            , ( "No", False )
            ]
                |> List.map (Tuple.mapSecond toBoolInstance)
                |> Just

        _ ->
            Nothing



-- [generator-start]


type alias GeoPoint =
    { lat : Float, lng : Float }


type alias ColorRgba =
    { red : Float, green : Float, blue : Float, alpha : Float }


type alias SpacingModel =
    { top : String
    , bottom : String
    , left : String
    , right : String
    }


type Kind
    = BoolKind
    | ListKind Kind
    | UnionKind UnionConstructors
    | ComplexKind (Dict Int NamedKind)
    | DateTimeKind
    | GeoPointKind
    | ImageKind
    | NumberKind
    | StringKind
    | NullKind
    | NullableKind Kind
    | ColorKind


type alias NamedKind =
    { name : String, type_ : Kind }


type alias NamedInstance =
    { name : String, instance : Instance }


type Instance
    = StringInstance String
    | NullInstance
    | NumberInstance Float
    | BoolInstance Bool
    | ComplexInstance (Dict Int NamedInstance)
    | ListInstance Kind (List Instance)
    | NullableInstance Kind (Maybe Instance)
    | GeoPointInstance GeoPoint
    | ImageInstance String
    | ColorInstance Color.Extra.Color
    | UnionInstance UnionConstructors UnionValue


type alias UnionValue =
    ( Int, List Instance )


type alias UnionConstructor =
    ( String, List Kind )


type alias UnionConstructors =
    IntDict.Dict UnionConstructor


type alias InstanceOption =
    ( String, Instance )


type alias InstanceOptions =
    List InstanceOption



-- [generator-generated-start] -- DO NOT MODIFY or remove this line


decodeColorRgba =
    Decode.map4
        ColorRgba
        (Decode.field "red" Decode.float)
        (Decode.field "green" Decode.float)
        (Decode.field "blue" Decode.float)
        (Decode.field "alpha" Decode.float)


decodeDictIntNamedInstance =
    let
        decodeDictIntNamedInstanceTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" Decode.int)
                (Decode.field "A2" decodeNamedInstance)
    in
    Decode.map Dict.fromList (Decode.list decodeDictIntNamedInstanceTuple)


decodeDictIntNamedKind =
    let
        decodeDictIntNamedKindTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" Decode.int)
                (Decode.field "A2" decodeNamedKind)
    in
    Decode.map Dict.fromList (Decode.list decodeDictIntNamedKindTuple)


decodeGeoPoint =
    Decode.map2
        GeoPoint
        (Decode.field "lat" Decode.float)
        (Decode.field "lng" Decode.float)


decodeInstance =
    Decode.lazy
        (\_ ->
            Decode.field "Constructor" Decode.string |> Decode.andThen decodeInstanceHelp
        )


decodeInstanceHelp constructor =
    case constructor of
        "StringInstance" ->
            Decode.map
                StringInstance
                (Decode.field "A1" Decode.string)

        "NullInstance" ->
            Decode.succeed NullInstance

        "NumberInstance" ->
            Decode.map
                NumberInstance
                (Decode.field "A1" Decode.float)

        "BoolInstance" ->
            Decode.map
                BoolInstance
                (Decode.field "A1" Decode.bool)

        "ComplexInstance" ->
            Decode.map
                ComplexInstance
                (Decode.field "A1" decodeDictIntNamedInstance)

        "ListInstance" ->
            Decode.map2
                ListInstance
                (Decode.field "A1" decodeKind)
                (Decode.field "A2" (Decode.list decodeInstance))

        "NullableInstance" ->
            Decode.map2
                NullableInstance
                (Decode.field "A1" decodeKind)
                (Decode.field "A2" (Decode.maybe decodeInstance))

        "GeoPointInstance" ->
            Decode.map
                GeoPointInstance
                (Decode.field "A1" decodeGeoPoint)

        "ImageInstance" ->
            Decode.map
                ImageInstance
                (Decode.field "A1" Decode.string)

        "ColorInstance" ->
            Decode.map
                ColorInstance
                (Decode.field "A1" Color.Extra.decodeColor)

        "UnionInstance" ->
            Decode.map2
                UnionInstance
                (Decode.field "A1" decodeUnionConstructors)
                (Decode.field "A2" decodeUnionValue)

        other ->
            Decode.fail <| "Unknown constructor for type Instance: " ++ other


decodeInstanceOption =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Decode.string)
        (Decode.field "A2" decodeInstance)


decodeInstanceOptions =
    Decode.list decodeInstanceOption


decodeKind =
    Decode.lazy
        (\_ ->
            Decode.field "Constructor" Decode.string |> Decode.andThen decodeKindHelp
        )


decodeKindHelp constructor =
    case constructor of
        "BoolKind" ->
            Decode.succeed BoolKind

        "ListKind" ->
            Decode.map
                ListKind
                (Decode.field "A1" decodeKind)

        "UnionKind" ->
            Decode.map
                UnionKind
                (Decode.field "A1" decodeUnionConstructors)

        "ComplexKind" ->
            Decode.map
                ComplexKind
                (Decode.field "A1" decodeDictIntNamedKind)

        "DateTimeKind" ->
            Decode.succeed DateTimeKind

        "GeoPointKind" ->
            Decode.succeed GeoPointKind

        "ImageKind" ->
            Decode.succeed ImageKind

        "NumberKind" ->
            Decode.succeed NumberKind

        "StringKind" ->
            Decode.succeed StringKind

        "NullKind" ->
            Decode.succeed NullKind

        "NullableKind" ->
            Decode.map
                NullableKind
                (Decode.field "A1" decodeKind)

        "ColorKind" ->
            Decode.succeed ColorKind

        other ->
            Decode.fail <| "Unknown constructor for type Kind: " ++ other


decodeNamedInstance =
    Decode.map2
        NamedInstance
        (Decode.field "name" Decode.string)
        (Decode.field "instance" decodeInstance)


decodeNamedKind =
    Decode.map2
        NamedKind
        (Decode.field "name" Decode.string)
        (Decode.field "type_" decodeKind)


decodeSpacingModel =
    Decode.map4
        SpacingModel
        (Decode.field "top" Decode.string)
        (Decode.field "bottom" Decode.string)
        (Decode.field "left" Decode.string)
        (Decode.field "right" Decode.string)


decodeUnionConstructor =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Decode.string)
        (Decode.field "A2" (Decode.list decodeKind))


decodeUnionConstructors =
    IntDict.decodeDict decodeUnionConstructor


decodeUnionValue =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" Decode.int)
        (Decode.field "A2" (Decode.list decodeInstance))


encodeColorRgba a =
    Encode.object
        [ ( "red", Encode.float a.red )
        , ( "green", Encode.float a.green )
        , ( "blue", Encode.float a.blue )
        , ( "alpha", Encode.float a.alpha )
        ]


encodeDictIntNamedInstance a =
    let
        encodeDictIntNamedInstanceTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.int a1 )
                , ( "A2", encodeNamedInstance a2 )
                ]
    in
    Encode.list encodeDictIntNamedInstanceTuple (Dict.toList a)


encodeDictIntNamedKind a =
    let
        encodeDictIntNamedKindTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.int a1 )
                , ( "A2", encodeNamedKind a2 )
                ]
    in
    Encode.list encodeDictIntNamedKindTuple (Dict.toList a)


encodeGeoPoint a =
    Encode.object
        [ ( "lat", Encode.float a.lat )
        , ( "lng", Encode.float a.lng )
        ]


encodeInstance a =
    case a of
        StringInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "StringInstance" )
                , ( "A1", Encode.string a1 )
                ]

        NullInstance ->
            Encode.object
                [ ( "Constructor", Encode.string "NullInstance" )
                ]

        NumberInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "NumberInstance" )
                , ( "A1", Encode.float a1 )
                ]

        BoolInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "BoolInstance" )
                , ( "A1", Encode.bool a1 )
                ]

        ComplexInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ComplexInstance" )
                , ( "A1", encodeDictIntNamedInstance a1 )
                ]

        ListInstance a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "ListInstance" )
                , ( "A1", encodeKind a1 )
                , ( "A2", Encode.list encodeInstance a2 )
                ]

        NullableInstance a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "NullableInstance" )
                , ( "A1", encodeKind a1 )
                , ( "A2", encodeMaybeInstance a2 )
                ]

        GeoPointInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "GeoPointInstance" )
                , ( "A1", encodeGeoPoint a1 )
                ]

        ImageInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ImageInstance" )
                , ( "A1", Encode.string a1 )
                ]

        ColorInstance a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ColorInstance" )
                , ( "A1", Color.Extra.encodeColor a1 )
                ]

        UnionInstance a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "UnionInstance" )
                , ( "A1", encodeUnionConstructors a1 )
                , ( "A2", encodeUnionValue a2 )
                ]


encodeInstanceOption ( a1, a2 ) =
    Encode.object
        [ ( "A1", Encode.string a1 )
        , ( "A2", encodeInstance a2 )
        ]


encodeInstanceOptions a =
    Encode.list encodeInstanceOption a


encodeKind a =
    case a of
        BoolKind ->
            Encode.object
                [ ( "Constructor", Encode.string "BoolKind" )
                ]

        ListKind a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ListKind" )
                , ( "A1", encodeKind a1 )
                ]

        UnionKind a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "UnionKind" )
                , ( "A1", encodeUnionConstructors a1 )
                ]

        ComplexKind a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "ComplexKind" )
                , ( "A1", encodeDictIntNamedKind a1 )
                ]

        DateTimeKind ->
            Encode.object
                [ ( "Constructor", Encode.string "DateTimeKind" )
                ]

        GeoPointKind ->
            Encode.object
                [ ( "Constructor", Encode.string "GeoPointKind" )
                ]

        ImageKind ->
            Encode.object
                [ ( "Constructor", Encode.string "ImageKind" )
                ]

        NumberKind ->
            Encode.object
                [ ( "Constructor", Encode.string "NumberKind" )
                ]

        StringKind ->
            Encode.object
                [ ( "Constructor", Encode.string "StringKind" )
                ]

        NullKind ->
            Encode.object
                [ ( "Constructor", Encode.string "NullKind" )
                ]

        NullableKind a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "NullableKind" )
                , ( "A1", encodeKind a1 )
                ]

        ColorKind ->
            Encode.object
                [ ( "Constructor", Encode.string "ColorKind" )
                ]


encodeMaybeInstance a =
    case a of
        Just b ->
            encodeInstance b

        Nothing ->
            Encode.null


encodeNamedInstance a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "instance", encodeInstance a.instance )
        ]


encodeNamedKind a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "type_", encodeKind a.type_ )
        ]


encodeSpacingModel a =
    Encode.object
        [ ( "top", Encode.string a.top )
        , ( "bottom", Encode.string a.bottom )
        , ( "left", Encode.string a.left )
        , ( "right", Encode.string a.right )
        ]


encodeUnionConstructor ( a1, a2 ) =
    Encode.object
        [ ( "A1", Encode.string a1 )
        , ( "A2", Encode.list encodeKind a2 )
        ]


encodeUnionConstructors a =
    IntDict.encodeDict encodeUnionConstructor a


encodeUnionValue ( a1, a2 ) =
    Encode.object
        [ ( "A1", Encode.int a1 )
        , ( "A2", Encode.list encodeInstance a2 )
        ]



-- [generator-end]
