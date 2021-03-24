module Element.Extra exposing (style)

import Element
import Html


style content =
    Html.node "style" [] [ Html.text content ]
        |> Element.html
