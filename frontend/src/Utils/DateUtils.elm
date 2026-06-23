module Utils.DateUtils exposing (..)

{-| Pure date helpers: calendar grids, Julian day arithmetic, and date formatting
-}

import Types exposing (Language(..), Today)


-- | Number of days in a month, with leap-year handling for February
daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if (modBy 4 year == 0 && modBy 100 year /= 0) || modBy 400 year == 0 then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


-- | Weekday as 0 (Sunday) … 6 (Saturday), using Sakamoto's compact algorithm
dayOfWeek : Int -> Int -> Int -> Int
dayOfWeek y m d =
    let
        t =
            [ 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 ]

        y2 =
            if m < 3 then
                y - 1

            else
                y

        ti =
            Maybe.withDefault 0 (List.head (List.drop (m - 1) t))
    in
    modBy 7 (y2 + y2 // 4 - y2 // 100 + y2 // 400 + ti + d)


-- | Weekday shift: 0 = Monday, used to align calendar grid columns
mondayFirst : Int -> Int -> Int -> Int
mondayFirst y m d =
    modBy 7 (dayOfWeek y m d + 6)


-- | Month grid: a list of weeks, each week a list of Maybe Int (Nothing = padding cell)
calendarWeeks : Int -> Int -> List (List (Maybe Int))
calendarWeeks year month =
    let
        numDays =
            daysInMonth year month

        firstDow =
            mondayFirst year month 1

        padStart =
            List.repeat firstDow Nothing

        days =
            List.map Just (List.range 1 numDays)

        all =
            padStart ++ days

        rem =
            modBy 7 (List.length all)

        padEnd =
            if rem == 0 then
                []

            else
                List.repeat (7 - rem) Nothing

        cells =
            all ++ padEnd
    in
    splitEvery 7 cells


-- | Split a list into chunks of n elements
splitEvery : Int -> List a -> List (List a)
splitEvery n list =
    if List.isEmpty list then
        []

    else
        List.take n list :: splitEvery n (List.drop n list)


-- | Return (year, month) for the month before the given one
prevMonth : Int -> Int -> ( Int, Int )
prevMonth y m =
    if m == 1 then
        ( y - 1, 12 )

    else
        ( y, m - 1 )


-- | Return (year, month) for the month after the given one
nextMonth : Int -> Int -> ( Int, Int )
nextMonth y m =
    if m == 12 then
        ( y + 1, 1 )

    else
        ( y, m + 1 )


-- | Localized full month name for the calendar header
monthName : Language -> Int -> String
monthName lang m =
    case lang of
        English ->
            case m of
                1 ->
                    "January"

                2 ->
                    "February"

                3 ->
                    "March"

                4 ->
                    "April"

                5 ->
                    "May"

                6 ->
                    "June"

                7 ->
                    "July"

                8 ->
                    "August"

                9 ->
                    "September"

                10 ->
                    "October"

                11 ->
                    "November"

                12 ->
                    "December"

                _ ->
                    ""

        Czech ->
            case m of
                1 ->
                    "Leden"

                2 ->
                    "Únor"

                3 ->
                    "Březen"

                4 ->
                    "Duben"

                5 ->
                    "Květen"

                6 ->
                    "Červen"

                7 ->
                    "Červenec"

                8 ->
                    "Srpen"

                9 ->
                    "Září"

                10 ->
                    "Říjen"

                11 ->
                    "Listopad"

                12 ->
                    "Prosinec"

                _ ->
                    ""


-- | Convert a Julian day number back to a Gregorian { year, month, day } record
fromJulian : Int -> { year : Int, month : Int, day : Int }
fromJulian jdn =
    let
        a =
            jdn + 32044

        b =
            (4 * a + 3) // 146097

        c =
            a - (b * 146097) // 4

        d_ =
            (4 * c + 3) // 1461

        e =
            c - (1461 * d_) // 4

        m =
            (5 * e + 2) // 153
    in
    { year = 100 * b + d_ - 4800 + m // 10
    , month = m + 3 - 12 * (m // 10)
    , day = e - (153 * m + 2) // 5 + 1
    }


-- | Format a date as "YYYY-MM-DD", used as a Dict key and for filter comparisons
dayKey : Int -> Int -> Int -> String
dayKey y m d =
    String.padLeft 4 '0' (String.fromInt y)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt m)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt d)


-- | Parse a "YYYY-MM-DD" string into { year, month, day }. returns Nothing on failure
dateParts : String -> Maybe Today
dateParts s =
    let
        y =
            String.slice 0 4 s |> String.toInt

        m =
            String.slice 5 7 s |> String.toInt

        d =
            String.slice 8 10 s |> String.toInt
    in
    case ( y, m, d ) of
        ( Just yy, Just mm, Just dd ) ->
            Just { year = yy, month = mm, day = dd }

        _ ->
            Nothing


-- | Convert a Gregorian date to a Julian day number for date arithmetic 
toJulian : Int -> Int -> Int -> Int
toJulian y m d =
    let
        a =
            (14 - m) // 12

        yy =
            y + 4800 - a

        mm =
            m + 12 * a - 3
    in
    d + (153 * mm + 2) // 5 + 365 * yy + yy // 4 - yy // 100 + yy // 400 - 32045
