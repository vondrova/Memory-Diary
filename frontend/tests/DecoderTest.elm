module DecoderTest exposing (..)

{-| Tests for all JSON decoders in `Api.Decode`.

Each decoder test feeds a raw JSON string and checks that the decoded Elm value
has the expected fields. This catches regressions when the backend changes its
response shape or a new decoder field is added.

Covered decoders:

  - `memoryDecoder` — full memory row (title, tags, photos, optional fields)
  - `importantDayDecoder` — birthday and manual day rows
  - `noteDecoder` — couple note with stable owner key
  - `statsDecoder` — aggregated statistics (counts, top lists, nullable title)
  - `diaryEntryDecoder` — personal diary entry (id, owner, date, body)
  - `profileDecoder` — person profile (side, name, display mode, optional photo)
  - `relationshipDecoder` — relationship row with computed countdown fields

-}

import Api.Codec exposing (diaryEntryDecoder, importantDayDecoder, memoryDecoder, noteDecoder, profileDecoder, relationshipDecoder, statsDecoder)
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "API Decoders"
        [ memoryDecoderTests
        , importantDayDecoderTests
        , noteDecoderTests
        , statsDecoderTests
        , relationshipDecoderTests
        , profileDecoderTests
        , diaryDecoderTests
        ]


importantDayDecoderTests : Test
importantDayDecoderTests =
    describe "important-day decoder"
        [ test "decodes explicit birthday metadata without a synthetic id" <|
            \_ ->
                """{"id":null,"title":"A","date":"2000-06-18T00:00:00Z","note":null,"kind":"birthday","owner":"left","nextOccurrence":"2026-06-18T00:00:00Z","daysUntil":4,"month":6,"day":18}"""
                    |> D.decodeString importantDayDecoder
                    |> Result.map
                        (\day ->
                            { id = day.id
                            , kind = day.kind
                            , owner = day.owner
                            , daysUntil = day.daysUntil
                            , month = day.month
                            , day = day.day
                            }
                        )
                    |> Expect.equal
                        (Ok
                            { id = Nothing
                            , kind = "birthday"
                            , owner = Just "left"
                            , daysUntil = 4
                            , month = 6
                            , day = 18
                            }
                        )
        ]


noteDecoderTests : Test
noteDecoderTests =
    describe "noteDecoder"
        [ test "decodes the stable owner key" <|
            \_ ->
                """{"id":1,"owner":"left","title":"Gift","body":"Book"}"""
                    |> D.decodeString noteDecoder
                    |> Result.map .owner
                    |> Expect.equal (Ok "left")
        ]



-- MEMORY


memoryDecoderTests : Test
memoryDecoderTests =
    describe "memoryDecoder"
        [ test "decodes title field" <|
            \_ ->
                """{"id":1,"title":"Trip","timeFrom":"2024-06-15T10:00","timeTo":"2024-06-15T18:00","description":"Fun","location":"Prague","tags":["travel"],"photos":[]}"""
                    |> D.decodeString memoryDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok "Trip")
        , test "decodes tags list" <|
            \_ ->
                """{"id":3,"title":"Party","timeFrom":"2024-06-15T20:00","timeTo":"2024-06-15T23:00","description":null,"location":null,"tags":["friends","fun","music"],"photos":[]}"""
                    |> D.decodeString memoryDecoder
                    |> Result.map .tags
                    |> Expect.equal (Ok [ "friends", "fun", "music" ])
        , test "decodes null description as Nothing" <|
            \_ ->
                """{"id":2,"title":"Walk","timeFrom":"2024-06-15T09:00","timeTo":"2024-06-15T10:00","description":null,"location":null,"tags":[],"photos":[]}"""
                    |> D.decodeString memoryDecoder
                    |> Result.map .description
                    |> Expect.equal (Ok Nothing)
        , test "decodes null location as Nothing" <|
            \_ ->
                """{"id":2,"title":"Walk","timeFrom":"2024-06-15T09:00","timeTo":"2024-06-15T10:00","description":null,"location":null,"tags":[],"photos":[]}"""
                    |> D.decodeString memoryDecoder
                    |> Result.map .location
                    |> Expect.equal (Ok Nothing)
        , test "decodes photos list" <|
            \_ ->
                """{"id":4,"title":"Snap","timeFrom":"2024-06-15T12:00","timeTo":"2024-06-15T13:00","description":null,"location":null,"tags":[],"photos":["a.jpg","b.png"]}"""
                    |> D.decodeString memoryDecoder
                    |> Result.map .photos
                    |> Expect.equal (Ok [ "a.jpg", "b.png" ])
        ]



-- STATS


statsDecoderTests : Test
statsDecoderTests =
    describe "statsDecoder"
        [ test "decodes memoryCount" <|
            \_ ->
                emptyStatsJson 5
                    |> D.decodeString statsDecoder
                    |> Result.map .memoryCount
                    |> Expect.equal (Ok 5)
        , test "decodes null longestTitle as Nothing" <|
            \_ ->
                emptyStatsJson 0
                    |> D.decodeString statsDecoder
                    |> Result.map .longestTitle
                    |> Expect.equal (Ok Nothing)
        , test "decodes totalMinutes" <|
            \_ ->
                """{"totalMinutes":120,"memoryCount":5,"photoCount":10,"visitedPlaces":3,"averageMinutes":24,"longestTitle":"Long trip","longestMinutes":60,"topTags":[["travel",3]],"topLocations":[["Prague",2]],"monthlyActivity":[["2024-06",5]],"dailyActivity":[["2024-06-15",2]]}"""
                    |> D.decodeString statsDecoder
                    |> Result.map .totalMinutes
                    |> Expect.equal (Ok 120)
        , test "decodes topTags pairs" <|
            \_ ->
                """{"totalMinutes":0,"memoryCount":0,"photoCount":0,"visitedPlaces":0,"averageMinutes":0,"longestTitle":null,"longestMinutes":0,"topTags":[["travel",3],["food",1]],"topLocations":[],"monthlyActivity":[],"dailyActivity":[]}"""
                    |> D.decodeString statsDecoder
                    |> Result.map .topTags
                    |> Expect.equal (Ok [ ( "travel", 3 ), ( "food", 1 ) ])
        ]


emptyStatsJson : Int -> String
emptyStatsJson count =
    """{"totalMinutes":0,"memoryCount":"""
        ++ String.fromInt count
        ++ ""","photoCount":0,"visitedPlaces":0,"averageMinutes":0,"longestTitle":null,"longestMinutes":0,"topTags":[],"topLocations":[],"monthlyActivity":[],"dailyActivity":[]}"""



-- DIARY


diaryDecoderTests : Test
diaryDecoderTests =
    describe "diaryEntryDecoder"
        [ test "decodes all fields" <|
            \_ ->
                """{"id":1,"owner":"left","date":"2026-06-21T00:00:00Z","body":"First entry"}"""
                    |> D.decodeString diaryEntryDecoder
                    |> Expect.equal
                        (Ok { id = 1, owner = "left", date = "2026-06-21T00:00:00Z", body = "First entry" })
        , test "decodes right owner" <|
            \_ ->
                """{"id":2,"owner":"right","date":"2026-01-01T00:00:00Z","body":"Hello"}"""
                    |> D.decodeString diaryEntryDecoder
                    |> Result.map .owner
                    |> Expect.equal (Ok "right")
        , test "fails on missing body field" <|
            \_ ->
                """{"id":3,"owner":"left","date":"2026-06-21T00:00:00Z"}"""
                    |> D.decodeString diaryEntryDecoder
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- RELATIONSHIP


relationshipDecoderTests : Test
relationshipDecoderTests =
    describe "relationshipDecoder"
        [ test "decodes heartColor" <|
            \_ ->
                """{"startDate":"2020-01-15T00:00:00Z","heartColor":"heart-red","daysTogether":1613,"nextAnniversary":"2025-01-15T00:00:00Z","anniversaryNumber":5,"nextMonthiversary":"2024-07-15T00:00:00Z","monthiversaryNum":54,"anniversaryDays":209,"monthiversaryDays":25}"""
                    |> D.decodeString relationshipDecoder
                    |> Result.map .heartColor
                    |> Expect.equal (Ok "heart-red")
        , test "decodes daysTogether" <|
            \_ ->
                """{"startDate":"2020-01-15T00:00:00Z","heartColor":"heart-red","daysTogether":1613,"nextAnniversary":"2025-01-15T00:00:00Z","anniversaryNumber":5,"nextMonthiversary":"2024-07-15T00:00:00Z","monthiversaryNum":54,"anniversaryDays":209,"monthiversaryDays":25}"""
                    |> D.decodeString relationshipDecoder
                    |> Result.map .daysTogether
                    |> Expect.equal (Ok (Just 1613))
        , test "decodes null startDate as Nothing" <|
            \_ ->
                """{"startDate":null,"heartColor":"heart-blue","daysTogether":null,"nextAnniversary":null,"anniversaryNumber":null,"nextMonthiversary":null,"monthiversaryNum":null,"anniversaryDays":null,"monthiversaryDays":null}"""
                    |> D.decodeString relationshipDecoder
                    |> Result.map .startDate
                    |> Expect.equal (Ok Nothing)
        , test "decodes null daysTogether as Nothing" <|
            \_ ->
                """{"startDate":null,"heartColor":"heart-blue","daysTogether":null,"nextAnniversary":null,"anniversaryNumber":null,"nextMonthiversary":null,"monthiversaryNum":null,"anniversaryDays":null,"monthiversaryDays":null}"""
                    |> D.decodeString relationshipDecoder
                    |> Result.map .daysTogether
                    |> Expect.equal (Ok Nothing)
        ]



-- PROFILE


profileDecoderTests : Test
profileDecoderTests =
    describe "profileDecoder"
        [ test "decodes name" <|
            \_ ->
                """{"side":"left","name":"A","displayMode":"figure","photo":null,"figureColor":"figure-red","accessory":"flower","expression":"smile","birthday":"1990-05-21T00:00:00Z"}"""
                    |> D.decodeString profileDecoder
                    |> Result.map .name
                    |> Expect.equal (Ok "A")
        , test "decodes null photo as Nothing" <|
            \_ ->
                """{"side":"left","name":"","displayMode":"figure","photo":null,"figureColor":"figure-red","accessory":"flower","expression":"smile","birthday":null}"""
                    |> D.decodeString profileDecoder
                    |> Result.map .photo
                    |> Expect.equal (Ok Nothing)
        , test "decodes side" <|
            \_ ->
                """{"side":"right","name":"B","displayMode":"photo","photo":"profile-b.jpg","figureColor":"figure-blue","accessory":"hat","expression":"laugh","birthday":null}"""
                    |> D.decodeString profileDecoder
                    |> Result.map .side
                    |> Expect.equal (Ok "right")
        ]
