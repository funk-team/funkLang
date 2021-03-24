module Calendar.Codec exposing (parseString)

import Calendar
import Parser exposing ((|.), (|=))
import Time


{-| Try to make sense of string input.
The idea is that it should recognize dates thrown at it in any format.
We can add more capabilities later on.

    import Parser
    import Calendar
    import Time

    Parser.run parseString "2020-01-01" |> Test.isOk

-}
parseString : Parser.Parser Calendar.Date
parseString =
    let
        raw : Parser.Parser Calendar.RawDate
        raw =
            Parser.succeed (\yyyy month dd -> { day = dd, month = month, year = yyyy })
                |= Parser.int
                |. Parser.keyword "-"
                |= (Parser.int |> Parser.andThen matchMonth)
                |. Parser.keyword "-"
                |= Parser.int
    in
    raw
        |> Parser.andThen
            (\rawDate ->
                case Calendar.fromRawParts rawDate of
                    Nothing ->
                        Parser.problem "Not a valid date"

                    Just date ->
                        Parser.succeed date
            )


{-| Assuming months are numbered 1-12 try turning an integer into a well-typed month.
-}
matchMonth : Int -> Parser.Parser Time.Month
matchMonth monthIndex =
    case monthIndex of
        1 ->
            Parser.succeed Time.Jan

        2 ->
            Parser.succeed Time.Feb

        3 ->
            Parser.succeed Time.Mar

        4 ->
            Parser.succeed Time.Apr

        5 ->
            Parser.succeed Time.May

        6 ->
            Parser.succeed Time.Jun

        7 ->
            Parser.succeed Time.Jul

        8 ->
            Parser.succeed Time.Aug

        9 ->
            Parser.succeed Time.Sep

        10 ->
            Parser.succeed Time.Oct

        11 ->
            Parser.succeed Time.Nov

        12 ->
            Parser.succeed Time.Dec

        num ->
            Parser.problem (String.fromInt num ++ " is not within 1-12. Dates must be 1-12")
