module SvgParser exposing
    ( SvgNode(..), Element, SvgAttribute
    , parseToNodes, nodeToSvg, toAttribute
    , parseToElementNode
    )

{-| String to SVG parser


# Primitives

@docs SvgNode, Element, SvgAttribute


# Parsing

@docs parse, parseToNodes, nodeToSvg, toAttribute

-}

import Combine exposing (..)
import Combine.Char exposing (anyChar)
import List.Extra
import Svg exposing (Attribute, Svg, node, text)
import VirtualDom exposing (attribute)


{-| A SVG node can be one of the three: SvgElement, SvgText or SvgComment.
-}
type SvgNode
    = SvgElement Element
    | SvgText String
    | SvgComment String


{-| An Element consists of a tag name, a list of attributes, a list of children nodes.

    <svg xmlns="http://www.w3.org/2000/svg"></svg>

will be parsed as

    Element "svg" [ ( "xmlns", "http://www.w3.org/2000/svg" ) ] []

-}
type alias Element =
    { name : String
    , attributes : List SvgAttribute
    , children : List SvgNode
    }


{-| A name/value pair to denote attribute.
-}
type alias SvgAttribute =
    ( String, String )



-- this function was removed in 0.19 of elm.


flip : (a -> b -> c) -> b -> a -> c
flip func b a =
    func a b



-- from deprecated operator (*>)


andMapRight : Parser s x -> Parser s a -> Parser s a
andMapRight lp rp =
    lp
        |> map (flip always)
        |> andMap rp



--from deprecated operator (<*)


andMapLeft : Parser s a -> Parser s x -> Parser s a
andMapLeft lp rp =
    lp
        |> map always
        |> andMap rp


attributeParser : Parser s SvgAttribute
attributeParser =
    andMap
        (optional "" <|
            andMapLeft
                (andMapRight
                    (string "=\"")
                    (regex "[^\"]*")
                )
            <|
                string "\""
        )
    <|
        map Tuple.pair <|
            regex "[^=>/]+"


openingParser : Parser s Element
openingParser =
    flip andMap
        (andMap
            (regex "[^/>\\s]+")
            (map
                (\_ tagName attributes ->
                    Element tagName attributes []
                )
                (string "<")
            )
        )
        (andMapLeft
            (andMapRight
                whitespace
                (sepBy whitespace attributeParser)
            )
            whitespace
        )


closingOrChildrenParser : Element -> Parser s Element
closingOrChildrenParser element =
    let
        childrenParser =
            map
                (\children -> { element | children = children })
                (andMapLeft
                    (andMapLeft
                        (andMapRight
                            (andMapRight whitespace
                                (string ">")
                            )
                            (many nodeParser)
                        )
                        whitespace
                    )
                    (string ("</" ++ element.name ++ ">"))
                )
    in
    lazy <|
        \_ ->
            choice
                [ andMapRight
                    (andMapRight
                        whitespace
                        (string "/>")
                    )
                    (succeed element)
                , childrenParser
                ]


elementParser : Parser s SvgNode
elementParser =
    lazy <|
        \_ ->
            map SvgElement
                (andThen closingOrChildrenParser
                    (whitespace
                        |> map (flip always)
                        |> andMap openingParser
                    )
                )


textParser : Parser s SvgNode
textParser =
    lazy <|
        \_ ->
            map
                SvgText
                (andMapRight
                    whitespace
                 <|
                    regex "[^<]+"
                )


commentParser : Parser s SvgNode
commentParser =
    lazy <|
        \_ ->
            map
                (SvgComment
                    << String.fromList
                )
                (andMapRight
                    (andMapRight
                        whitespace
                     <|
                        string "<!--"
                    )
                 <|
                    manyTill anyChar <|
                        string "-->"
                )


nodeParser : Parser s SvgNode
nodeParser =
    lazy <|
        \_ ->
            choice
                [ textParser
                , commentParser
                , elementParser
                ]


{-| Convert `SvgAttribute` to `Attribute msg`. This is useful when you want to manipulate `SvgAttribute` before converting to `Html msg`.
-}
toAttribute : SvgAttribute -> Attribute msg
toAttribute ( name, value ) =
    attribute name value


elementToSvg : Element -> Svg msg
elementToSvg element =
    node element.name
        (List.map toAttribute element.attributes)
        (List.map nodeToSvg element.children)


{-| Convert `SvgNode` to `Svg msg`. This is useful when you want to manipulate `SvgNode` before conveting to `Html msg`.
-}
nodeToSvg : SvgNode -> Svg msg
nodeToSvg svgNode =
    case svgNode of
        SvgElement element ->
            elementToSvg element

        SvgText content ->
            text content

        SvgComment content ->
            text ""


{-| Parse xml declaration
-}
xmlDeclarationParser : Parser s String
xmlDeclarationParser =
    map
        String.fromList
        (andMapRight
            (andMapRight whitespace <|
                string "<?xml"
            )
         <|
            manyTill anyChar (string "?>")
        )


normalizeQuotes =
    String.replace "'" "\""


{-| Parses `String` to `SvgNode`. Normally you will use `parse` instead of this.

    parseToNodes "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"
    --> Ok [SvgElement (Element "svg" [ ( "xmlns", "http://www.w3.org/2000/svg" ) ] [])]

    parseToNodes "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"-2 -1.5 24 24\" width=\"24\" height=\"24\" preserveAspectRatio=\"xMinYMin\" class=\"jam jam-circle-f\"><path d='M10 20.565c-5.523 0-10-4.477-10-10s4.477-10 10-10 10 4.477 10 10-4.477 10-10 10z' /></svg>"
    --> Ok [SvgElement { attributes = [("xmlns","http://www.w3.org/2000/svg"),("viewBox","-2 -1.5 24 24"),("width","24"),("height","24"),("preserveAspectRatio","xMinYMin"),("class","jam jam-circle-f")], children = [SvgElement { attributes = [("d","M10 20.565c-5.523 0-10-4.477-10-10s4.477-10 10-10 10 4.477 10 10-4.477 10-10 10z")], children = [], name = "path" }], name = "svg" }]

-}
parseToNodes : String -> Result String (List SvgNode)
parseToNodes input =
    case
        Combine.runParser
            (andMapRight
                (optional "" xmlDeclarationParser)
                (Combine.many1 nodeParser)
            )
            []
            (input |> normalizeQuotes)
    of
        Ok ( _, _, svgNode ) ->
            Ok svgNode

        Err ( _, stream, errors ) ->
            Err <| String.join " or " errors


parseToElementNode : String -> Result String SvgNode
parseToElementNode =
    parseToNodes >> Result.andThen findElementNode


findElementNode : List SvgNode -> Result String SvgNode
findElementNode nodes =
    case
        List.Extra.find
            (\node ->
                case node of
                    SvgElement _ ->
                        True

                    _ ->
                        False
            )
            nodes
    of
        Just node ->
            Ok node

        Nothing ->
            Err "No valid SVG Elements found, only text or comment nodes"



-- {-| Parses `String` to `Html msg`. Usually this is the only function you need.
-- -}
-- parse : String -> Result String (Html msg)
-- parse input =
--     let
--         toHtml svgNode =
--             case svgNode of
--                 SvgElement element ->
--                     if element.name == "svg" then
--                         Ok <|
--                             svg (List.map toAttribute element.attributes)
--                                 (List.map nodeToSvg element.children)
--
--                     else
--                         Err "Top element is not svg"
--
--                 _ ->
--                     Err "Top element is not svg"
--     in
--     parseToNodes input |> Result.andThen (List.map toHtml)
