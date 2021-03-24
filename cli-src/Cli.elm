module Cli exposing (main)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState
import Cli.Ports
import Cli.Program as Program


type FunkOptions
    = Serve



-- = Generate String
--generateOptionsParser : OptionsParser.OptionsParser FunkOptions Cli.OptionsParser.BuilderState.AnyOptions
--generateOptionsParser =
--    OptionsParser.buildSubCommand "generate" Generate
--        |> OptionsParser.with (Option.requiredPositionalArg "component name")
--        |> OptionsParser.withDoc "Generate a new custom element"


programConfig : Program.Config FunkOptions
programConfig =
    Program.config
        -- |> Program.add generateOptionsParser
        |> Program.add (OptionsParser.buildSubCommand "dev" Serve)


init : Flags -> FunkOptions -> Cmd Never
init flags options =
    case options of
        -- Generate componentName ->
        --     Cli.Ports.generate componentName
        Serve ->
            Cli.Ports.dev ()


type alias Flags =
    Program.FlagsIncludingArgv {}


main : Program.StatelessProgram Never {}
main =
    Program.stateless
        { printAndExitFailure = Cli.Ports.printAndExitFailure
        , printAndExitSuccess = Cli.Ports.printAndExitSuccess
        , init = init
        , config = programConfig
        }
