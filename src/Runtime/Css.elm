module Runtime.Css exposing (render)

import Html
import Html.Attributes


render =
    Html.node "style" [ Html.Attributes.type_ "text/css" ] [ Html.text css ]


css =
    """
/* RUNTIME CSS */
/* styling funk images */
[id^='el'] > img {
    width: 100%;
    height: 100%;
    display: block;
    object-fit: cover;
}

/* icons should always be contained */
[id^='el'].funk-icon > img {
    object-fit: contain;
}
[id^='el'].object-fit-contain.object-fit-contain.object-fit-contain.object-fit-contain.object-fit-contain
    > img {
    object-fit: contain;
}
[id^='el'].object-fit-cover.object-fit-cover.object-fit-cover.object-fit-cover.object-fit-cover
    > img {
    object-fit: cover;
}
.object-fit-contain.object-fit-contain.object-fit-contain.object-fit-contain.object-fit-contain
    {
    object-fit: contain;
    background-size: contain;
}
.object-fit-cover.object-fit-cover.object-fit-cover.object-fit-cover.object-fit-cover
    {
    object-fit: cover;
    background-size: cover;
}

/* Set the default style to wrap and respect user-defined whitespace */
.funk-text .t.t.t {
    white-space: pre-wrap
}

/* Override the default wrapping behavior with whitespace-respecting nowrap */
.white-space-nowrap.white-space-nowrap.white-space-nowrap ,
.white-space-nowrap.white-space-nowrap.white-space-nowrap .t.t.t {
    white-space: pre
}

/* high-specificity required to overrride the elm-ui default styles */
.pointer-events.pointer-events.pointer-events > * {
    pointer-events: inherit;
}
.pointer-events.pointer-events.pointer-events.none {
    pointer-events: none;
}
.pointer-events.pointer-events.pointer-events.all {
    pointer-events: all;
}

.custom-line-height .p.p.p.p {
    line-height: inherit;
}

"""
