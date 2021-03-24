module Debug.Extra exposing (..)

import Json.Decode as Decode


loggingDecoder : (a -> a) -> Decode.Decoder a -> Decode.Decoder a
loggingDecoder log realDecoder =
    Decode.value
        |> Decode.andThen
            (\event ->
                case Decode.decodeValue realDecoder event of
                    Ok decoded ->
                        Decode.succeed decoded

                    Err error ->
                        error
                            |> Decode.errorToString
                            |> log
                            |> Decode.fail
            )
