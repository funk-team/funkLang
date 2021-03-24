port module Cli.Ports exposing (..)


port print : String -> Cmd msg


port printAndExitFailure : String -> Cmd msg


port printAndExitSuccess : String -> Cmd msg


port dev : () -> Cmd msg


port generate : String -> Cmd msg
