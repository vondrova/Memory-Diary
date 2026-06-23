module HelpersTest exposing (..)

{-| Tests for pure helper functions in `Helpers` and `DateUtils`

Covered areas:
  - `formatDate` / `formatDuration` — date and duration display formatting
  - `dayKey`                        — zero-padded "MM-DD" key used for
                                      birthday and anniversary lookups
  - Photo filename safety            — `safePhotoName` rejects path-like and
                                       URL-like filenames
  - Form validation helpers          — `importantDayFormValid`, `noteFormValid`,
                                       `planFormValid` with various invalid inputs
-}

import Utils.DateUtils exposing (dayKey, daysInMonth)
import Expect
import Utils.Helpers exposing (..)
import Test exposing (Test, describe, test)
import Types exposing (Language(..), Memory)


suite : Test
suite =
    describe "Helpers"
        [ formattingTests
        , photoNameTests
        , formValidationTests
        , calendarTests
        , profileTests
        , zodiacTests
        , isBlankTests
        , uniqueStringsTests
        , countdownTests
        , urlTests
        , visitedPlacesTests
        , memoriesOnDayTests
        ]



-- FORMATTING


formattingTests : Test
formattingTests =
    describe "formatting"
        [ test "formatDate formats ISO date as DD. MM. YYYY" <|
            \_ ->
                formatDate "2024-06-15"
                    |> Expect.equal "15. 06. 2024"
        , test "formatDate truncates time portion" <|
            \_ ->
                formatDate "2024-06-15T10:30:00Z"
                    |> Expect.equal "15. 06. 2024"
        , test "formatDuration minutes only" <|
            \_ ->
                formatDuration 45
                    |> Expect.equal "45 min"
        , test "formatDuration zero minutes" <|
            \_ ->
                formatDuration 0
                    |> Expect.equal "0 min"
        , test "formatDuration hours and minutes" <|
            \_ ->
                formatDuration 90
                    |> Expect.equal "1 h 30 min"
        , test "formatDuration days hours minutes" <|
            \_ ->
                formatDuration (2 * 60 * 24 + 3 * 60 + 15)
                    |> Expect.equal "2 d 3 h 15 min"
        , test "dayKey pads single-digit month and day" <|
            \_ ->
                dayKey 2024 6 5
                    |> Expect.equal "2024-06-05"
        , test "dayKey does not alter double-digit values" <|
            \_ ->
                dayKey 2024 12 25
                    |> Expect.equal "2024-12-25"
        ]


photoNameTests : Test
photoNameTests =
    describe "photo filenames"
        [ test "accepts plain stored image filenames" <|
            \_ ->
                isSafePhotoName "20260615120000000000-1.HEIC"
                    |> Expect.equal True
        , test "rejects path-like photo names" <|
            \_ ->
                isSafePhotoName "../secret.jpg"
                    |> Expect.equal False
        , test "rejects URL-like photo names" <|
            \_ ->
                isSafePhotoName "photo.jpg?raw=1"
                    |> Expect.equal False
        ]



-- CALENDAR


formValidationTests : Test
formValidationTests =
    describe "form validation"
        [ test "importantDayFormValid requires title and date" <|
            \_ ->
                importantDayFormValid validImportantDayForm
                    |> Expect.equal True
        , test "importantDayFormValid rejects a blank title" <|
            \_ ->
                importantDayFormValid { validImportantDayForm | title = " " }
                    |> Expect.equal False
        , test "noteFormValid requires a stable owner, title and body" <|
            \_ ->
                noteFormValid validNoteForm
                    |> Expect.equal True
        , test "noteFormValid rejects an unknown owner" <|
            \_ ->
                noteFormValid { validNoteForm | owner = "A" }
                    |> Expect.equal False
        , test "planFormValid requires category and title" <|
            \_ ->
                planFormValid validPlanForm
                    |> Expect.equal True
        , test "planFormValid rejects a blank title" <|
            \_ ->
                planFormValid { validPlanForm | title = " " }
                    |> Expect.equal False
        ]


validImportantDayForm =
    { open = True
    , editId = Nothing
    , title = "Birthday"
    , date = "2026-06-15"
    , note = ""
    , showValidation = False
    }


validNoteForm =
    { open = True
    , editId = Nothing
    , owner = "shared"
    , title = "Favorite movie"
    , body = "Arrival"
    , showValidation = False
    }


validPlanForm =
    { open = True
    , editId = Nothing
    , category = "Trips"
    , catSuggestOpen = False
    , title = "Visit Brno"
    , detail = ""
    , done = False
    , showValidation = False
    }


calendarTests : Test
calendarTests =
    describe "calendar helpers"
        [ test "daysInMonth: February in leap year has 29 days" <|
            \_ ->
                daysInMonth 2024 2
                    |> Expect.equal 29
        , test "daysInMonth: February in non-leap year has 28 days" <|
            \_ ->
                daysInMonth 2023 2
                    |> Expect.equal 28
        , test "daysInMonth: 1900 is not a leap year (divisible by 100 not 400)" <|
            \_ ->
                daysInMonth 1900 2
                    |> Expect.equal 28
        , test "daysInMonth: 2000 is a leap year (divisible by 400)" <|
            \_ ->
                daysInMonth 2000 2
                    |> Expect.equal 29
        , test "daysInMonth: April has 30 days" <|
            \_ ->
                daysInMonth 2024 4
                    |> Expect.equal 30
        , test "daysInMonth: January has 31 days" <|
            \_ ->
                daysInMonth 2024 1
                    |> Expect.equal 31
        ]



-- PROFILE


profileTests : Test
profileTests =
    describe "profileDataToAvatar"
        [ test "real birthday keeps first 10 characters (YYYY-MM-DD)" <|
            \_ ->
                profileDataToAvatar
                    { side = "left"
                    , name = "A"
                    , displayMode = "figure"
                    , photo = Nothing
                    , figureColor = "figure-red"
                    , accessory = "flower"
                    , expression = "smile"
                    , birthday = Just "1990-05-21T00:00:00Z"
                    }
                    |> .birthday
                    |> Expect.equal "1990-05-21"
        , test "Nothing birthday becomes empty string" <|
            \_ ->
                profileDataToAvatar
                    { side = "right"
                    , name = "B"
                    , displayMode = "photo"
                    , photo = Just "bob.jpg"
                    , figureColor = "figure-blue"
                    , accessory = "hat"
                    , expression = "laugh"
                    , birthday = Nothing
                    }
                    |> .birthday
                    |> Expect.equal ""
        , test "Nothing photo becomes empty string" <|
            \_ ->
                profileDataToAvatar
                    { side = "left"
                    , name = "A"
                    , displayMode = "figure"
                    , photo = Nothing
                    , figureColor = "figure-red"
                    , accessory = "flower"
                    , expression = "smile"
                    , birthday = Nothing
                    }
                    |> .photo
                    |> Expect.equal ""
        ]



-- ZODIAC


zodiacTests : Test
zodiacTests =
    describe "zodiacSymbol"
        [ test "March 21 is Aries" <|
            \_ ->
                zodiacSymbol "1990-03-21"
                    |> Expect.equal "♈"
        , test "June 15 is Gemini" <|
            \_ ->
                zodiacSymbol "1990-06-15"
                    |> Expect.equal "♊"
        , test "December 25 is Capricorn" <|
            \_ ->
                zodiacSymbol "1990-12-25"
                    |> Expect.equal "♑"
        , test "empty string returns dot placeholder" <|
            \_ ->
                zodiacSymbol ""
                    |> Expect.equal "·"
        , test "zodiacName: March 21 is Aries" <|
            \_ ->
                zodiacName "1990-03-21"
                    |> Expect.equal "Aries"
        , test "zodiacName: June 15 is Gemini" <|
            \_ ->
                zodiacName "1990-06-15"
                    |> Expect.equal "Gemini"
        , test "zodiacName: empty string returns empty" <|
            \_ ->
                zodiacName ""
                    |> Expect.equal ""
        ]



-- IS BLANK


isBlankTests : Test
isBlankTests =
    describe "isBlank"
        [ test "empty string is blank" <|
            \_ ->
                isBlank ""
                    |> Expect.equal True
        , test "whitespace-only string is blank" <|
            \_ ->
                isBlank "   "
                    |> Expect.equal True
        , test "non-empty string is not blank" <|
            \_ ->
                isBlank "hello"
                    |> Expect.equal False
        , test "string with leading whitespace is not blank" <|
            \_ ->
                isBlank "  hello  "
                    |> Expect.equal False
        ]



-- UNIQUE STRINGS


uniqueStringsTests : Test
uniqueStringsTests =
    describe "uniqueStrings"
        [ test "empty list stays empty" <|
            \_ ->
                uniqueStrings []
                    |> Expect.equal []
        , test "list with no duplicates keeps all elements" <|
            \_ ->
                uniqueStrings [ "a", "b", "c" ]
                    |> List.sort
                    |> Expect.equal [ "a", "b", "c" ]
        , test "duplicates are removed, last occurrence kept" <|
            \_ ->
                uniqueStrings [ "trip", "summer", "trip" ]
                    |> List.length
                    |> Expect.equal 2
        , test "all-same list reduces to a singleton" <|
            \_ ->
                uniqueStrings [ "tag", "tag", "tag" ]
                    |> List.length
                    |> Expect.equal 1
        ]



-- COUNTDOWN


countdownTests : Test
countdownTests =
    describe "countdownShort"
        [ test "0 days returns Today!" <|
            \_ ->
                countdownShort English 0
                    |> Expect.equal "Today!"
        , test "1 day returns Tomorrow" <|
            \_ ->
                countdownShort English 1
                    |> Expect.equal "Tomorrow"
        , test "multiple days returns day count string" <|
            \_ ->
                countdownShort English 7
                    |> Expect.equal "in 7 days"
        , test "Czech: 0 days" <|
            \_ ->
                countdownShort Czech 0
                    |> Expect.equal "Dnes!"
        ]



-- URL HELPERS


urlTests : Test
urlTests =
    describe "URL helpers"
        [ test "photoUrl prepends /photos/ prefix" <|
            \_ ->
                photoUrl "20260615120000-1.jpg"
                    |> Expect.equal "/photos/20260615120000-1.jpg"
        , test "mapsUrl encodes spaces as plus signs" <|
            \_ ->
                mapsUrl "New York"
                    |> Expect.equal "https://www.google.com/maps/search/?api=1&query=New+York"
        , test "mapsUrl trims leading and trailing whitespace" <|
            \_ ->
                mapsUrl "  Prague  "
                    |> Expect.equal "https://www.google.com/maps/search/?api=1&query=Prague"
        ]



-- VISITED PLACES


visitedPlacesTests : Test
visitedPlacesTests =
    describe "visitedPlacesCount"
        [ test "empty list returns 0" <|
            \_ ->
                visitedPlacesCount []
                    |> Expect.equal 0
        , test "counts unique locations" <|
            \_ ->
                visitedPlacesCount
                    [ sampleMemory (Just "Prague")
                    , sampleMemory (Just "Vienna")
                    , sampleMemory (Just "Prague")
                    ]
                    |> Expect.equal 2
        , test "is case-insensitive" <|
            \_ ->
                visitedPlacesCount
                    [ sampleMemory (Just "Prague")
                    , sampleMemory (Just "prague")
                    ]
                    |> Expect.equal 1
        , test "ignores memories without a location" <|
            \_ ->
                visitedPlacesCount
                    [ sampleMemory Nothing
                    , sampleMemory (Just "Vienna")
                    ]
                    |> Expect.equal 1
        ]



-- MEMORIES ON DAY


memoriesOnDayTests : Test
memoriesOnDayTests =
    describe "memoriesOnDay"
        [ test "returns memories whose timeFrom starts with the given day key" <|
            \_ ->
                memoriesOnDay
                    [ sampleMemoryAt "2026-06-15T10:00:00Z"
                    , sampleMemoryAt "2026-06-16T09:00:00Z"
                    , sampleMemoryAt "2026-06-15T18:00:00Z"
                    ]
                    "2026-06-15"
                    |> List.length
                    |> Expect.equal 2
        , test "returns empty list when no memories match" <|
            \_ ->
                memoriesOnDay
                    [ sampleMemoryAt "2026-06-15T10:00:00Z" ]
                    "2026-06-16"
                    |> Expect.equal []
        ]


sampleMemory : Maybe String -> Memory
sampleMemory loc =
    { id = 1
    , title = "Trip"
    , timeFrom = "2026-06-15T10:00:00Z"
    , timeTo = "2026-06-15T18:00:00Z"
    , description = Nothing
    , location = loc
    , tags = []
    , photos = []
    }


sampleMemoryAt : String -> Memory
sampleMemoryAt timeFrom =
    { id = 1
    , title = "Trip"
    , timeFrom = timeFrom
    , timeTo = timeFrom
    , description = Nothing
    , location = Nothing
    , tags = []
    , photos = []
    }
