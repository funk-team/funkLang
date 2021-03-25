module Canvas.Tool.Responsify.Doc exposing (..)

{-| The responsify engine takes as input a set of drawn rectangles,
arranged as a containing parent rectangle and an unorder collection of
child rectangles contained within the parent. It uses a set of rules
to try to determine the intended layout represented by those static
rectangles, and then, if it finds a suitable rule, it applies padding,
spacing and layout transformations on the child and parent elements to
turn the static layout into a dynamic flow layout the behaves
suitably.

The top-level function to run the responsify algorithm is
`Canvas.Tool.Responsify.Engine.runResponsifyEngine` which returns a
tuple of a `Maybe String` giving the name of the rule that was
applied, a cache of information collected during the rule analysis
(type `Info` from `Canvas.Tool.Responsify.Info`) and the generated
flow element representing the responsified layout.


# Rules

Rules are composed of two parts:

  - A _matcher_, which decides whether the rule is applicable to a
    particular layout of static elements.

  - A _transform_, which is applied if the rule is found to be
    applicable, and which turns the static layout of rectangles into a
    dynamic flow layout.


# Matching with facts

The matcher part of a rule works by collecting a set of information
about the static layout. This information comes in three types:

  - _Facts_ are boolean-valued information, e.g. are all the child
    elements horizontally centred in the parent element?, do the
    elements overlap in either the x- or y-direction?, and so on.

  - _Parameters_ are integer-valued information, e.g. if the child
    elements are divided into two distinct groups in one direction, how
    many elements are in the first group and how many in the second?

  - _Dimensions_ are float-valued information, e.g. what is the minimum
    spacing between elements in the "along-axis" direction?

As this information is collected, each particular piece of information
is memoised using a simple state monad, implemented in
`Canvas.Tool.Responsify.Info`. This means that multiple rules using
the same pieces of information do not incur a double cost for
calculating the information. (This is important, in particular, for
spacing information, which requires more analysis than some of the
other kinds of information.)

Facts generally apply to a layout as a whole, rather than to a single
element. As a concrete example, there is a fact called
`AllHorizSideAligned` that's true if each element is aligned to either
the left or the right side of the parent. That fact is checked by a
simple fold over the child elements: basically, `List.foldl (&&) True
<| List.map check data.children` where `check` is a function that
checks a single child for alignment to the edges of the parent). The
rule has other facts that have to be true for it to fire, but they're
all computed in the same sort of way.

Rules are evaluated in the order specified in the `rules` list in
`Canvas.Tool.Responsify.Engine`, and the first applicable rule is used
to transform the layout.

**NOTE**: this means that the ordering of rules is significant!


# Transforming static layouts to dynamic layouts

Once a rule has been selected for application, the algorithm switches
to a different mode of operation where it applies the transformation
for the selected rule to the absolutely positioned elements from the
input layout.

The `Canvas.Tool.Responsify.Transforms` module defines a number of
functions that set up alignment, size and so on for each child element
in a layout. These functions are all specialisations of a generic
`transform` function, which takes the following arguments:

  - A _configuration record_, which defines the flow direction (row or
    column), spacing, a sort criterion for ordering child elements and
    a function to modify padding information for specific cases.

  - A _flow builder_, which is a function that is passed information
    about each child in turn and returns the alignment and size
    information for the child. The flow builder function is also passed
    the information collected during the rule matching process, since
    this is useful for some layouts. For some rules, extra information
    is needed for each child element to make the determination of
    positioning.

  - An _extras generator_, which is a function of type `TransformExtras
    a`, that creates the extra information needed by the flow builder.

For example, there is a specialisation called
`indexedColumnTransform`, which generates a column layout and passes
the index of each child element (sorted top-to-bottom) to the flow
builder function.


# How to think about this

In general, I'd say that recognising rules works on layouts, and
applying rules works on elements, using information derived from the
layout as a whole. (That's definitely true for all the rules so far.)

The reason for thinking about things in these terms is that it really
does only make sense to think of layouts as a whole for matching.
Identifying particular layouts depends on features like even sizing or
spacing of child elements, which can't be determined by looking at a
single child element, but only by looking at the layout as a whole.

Once the layout is decided though, each child element needs to be
transformed individually, even though those transformations often rely
on information determined from the layout as a whole during the
matching phase.


# Rule case examples

A number of special case rules are defined. Two examples are given
here, but the rest can be seen in the modules with names of the form
`Canvas.Tool.Responsify.Rules.Foo`.


## Singleton

The simplest case is the situation where a parent element has a single
child. If the child is drawn centred in either direction in the
parent, then it is aligned to the centre of the parent in the output
layout. If not, then the child is attached to the closest parent edge,
giving a sort of "gravity" effect (in the sense that that word is
usually used in GUI layout). This gives behaviour that feels natural
when the parent element is resized.

The matcher for this rule is simple: is there a single child element?
However, additional informatiojn is collected during the matching
process to determine the gravity of the child element within the
parent, and this information is later used during the transformation
process to decide on how to align and pad the child within the parent.


## Gapped row

A more complex layout is when there is a row of child elements that
can be divided into two distinct groups with an identifiable gap
between the groups. Here, a row is defined as a sequence of elements
that do not overlap in the X-direction and whose centroids are
approximately aligned in the Y-direction.

In this case, the elements of the first group should be aligned left
and the elements of the second group aligned right, with suitable
padding.

The matcher here needs to determine whether the layout is a row
(simple), and whether there are two distinct groups of child elements.
That process relies on the `analyseSpacing` function in
`Canvas.Tool.Responsify.Facts.Spacing`, which performs a general
along-axis spacing analysis of the child elements and collects
information about group sizes and spacing as it does so. That
information is memoised and is used later in the transform step.


# General principles for fallback cases

If none of the special cases apply, but the layout is identifiable as
a row or column, then an appropriate fallback rule is applied. These
rules (should) follow these general principles (_this is still being
worked on, so these principles should be viewed as aspirational for
the moment_):

1.  Applying responsify should not change the size, position or spacing
    of user-drawn elements, unless the positions of the elements are
    close to alignment (side or centre) in the parent element and
    applying responsive settings would only move the elements a small
    distance.

2.  Fallback row or column layouts should ensure that elements remain
    the same size as they were drawn with the correct spacing.

3.  If it is possible to position an element using alignment, padding
    should not be applied. Also, David says: "There should almost
    always be alignment settings instead of padding, positioning things
    with padding results in the element not being responsive and
    difficult to position."

4.  Dimensions should shrink to the size they are drawn, except in
    cases where there would be empty space the element could take up,
    in which case they should fill.

    David: If the element is going to fill it shouldn’t be bigger then
    what it is drawn as. I’m thinking when you have have a row of three
    things the last one is should probably be fill as when the user
    expands the screen they can see the last element is reacting to
    what they are doing.


# Possible approaches for clustering

The most general way I've been thinking of clustering goes something
like this:

  - If the layout is identified as a row or column but doesn't match
    any of the predefined layout cases, then try _along axis slicing_,
    which means trying to find ways of cutting up the child elements
    into groups with equal spacing. Those groups can then be injected
    into clusters.

  - If the layout isn't detected as a row or column (for example,
    imagine a layout with a row of buttons at the top, and a big
    drawing area below), then try _orthogonal slicing_, which means
    trying to partition the children to one side or another of a
    horizontal or vertical line across the parent. If the children on
    one side of the slicing line can be recognised as a row or column
    then cluster them, apply responsify to them and start over from the
    top (to capture cases where there's a column of rows or similar).
    If there's no good slicing (with "good" being defined as "try for a
    bit then give up"), say that there's no recognisable layout.

I think that approach will capture most of the cases where you have
something like a navbar, a sidebar, normal form layouts, and so on.

-}
