module Util exposing (pipe)

{-| Chain a list of functions
-}


pipe fns arg =
    List.foldl
        (\update_ model_ -> update_ model_)
        arg
        fns
