module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule

    --, NoUnused.CustomTypeConstructors.rule []
    --, NoUnused.CustomTypeConstructorArgs.rule
    --, NoUnused.Dependencies.rule
    --, NoUnused.Exports.rule
    --, NoUnused.Modules.rule
    --, NoUnused.Parameters.rule
    --, NoUnused.Patterns.rule
    ]
        |> List.map (Rule.ignoreErrorsForFiles [ "src/Viewport.elm", "src/Ui/Boxicons", "src/Ui/EntypoIcons.elm" ])
        |> List.map (Rule.ignoreErrorsForDirectories [ "tests/VerifyExamples" ])
