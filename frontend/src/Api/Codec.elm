{- JSON decoders and encoders for all API types.
   Field names match the backend JSON produced by aesonOpts:
   record prefixes are stripped and the first character is lowercased,
   so memoryTitle becomes "title", inputTimeFrom becomes "timeFrom", etc.
-}
module Api.Codec exposing
    ( memoryDecoder
    , importantDayDecoder
    , coupleNoteDecoder
    , couplePlanDecoder
    , diaryDecoder
    , geoDecoder
    , statsDecoder
    , encodeMemoryInput
    , encodeImportantDayInput
    , encodeNoteInput
    , encodePlanInput
    , encodeDiaryInput
    )

import Json.Decode as D
import Json.Encode as E
import Types exposing (..)


memoryDecoder : D.Decoder Memory
memoryDecoder =
    D.map8 Memory
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "timeFrom" D.string)
        (D.field "timeTo" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "location" D.string))
        (D.field "tags" (D.list D.string))
        (D.field "photos" (D.list D.string))


importantDayDecoder : D.Decoder ImportantDay
importantDayDecoder =
    D.map5 ImportantDay
        (D.maybe (D.field "id" D.int))
        (D.field "title" D.string)
        (D.field "date" D.string)
        (D.maybe (D.field "note" D.string))
        (D.field "kind" D.string)


coupleNoteDecoder : D.Decoder CoupleNote
coupleNoteDecoder =
    D.map5 CoupleNote
        (D.maybe (D.field "id" D.int))
        (D.field "owner" D.string)
        (D.field "title" D.string)
        (D.field "body" D.string)
        (D.field "createdAt" D.string)


couplePlanDecoder : D.Decoder CouplePlan
couplePlanDecoder =
    D.map5 CouplePlan
        (D.maybe (D.field "id" D.int))
        (D.field "category" D.string)
        (D.field "title" D.string)
        (D.maybe (D.field "detail" D.string))
        (D.field "done" D.bool)


diaryDecoder : D.Decoder DiaryEntry
diaryDecoder =
    D.map5 DiaryEntry
        (D.maybe (D.field "id" D.int))
        (D.field "date" D.string)
        (D.maybe (D.field "mood" D.string))
        (D.field "body" D.string)
        (D.maybe (D.field "weather" D.string))


geoDecoder : D.Decoder GeoSuggestion
geoDecoder =
    D.map3 GeoSuggestion
        (D.field "displayName" D.string)
        (D.field "lat" D.string)
        (D.field "lon" D.string)


statsDecoder : D.Decoder Stats
statsDecoder =
    D.map7 Stats
        (D.field "totalMinutes" D.int)
        (D.field "memoryCount" D.int)
        (D.field "photoCount" D.int)
        (D.field "visitedPlaces" D.int)
        (D.field "averageMinutes" D.int)
        (D.field "topTags" (D.list tagPairDecoder))
        (D.field "topLocations" (D.list tagPairDecoder))


tagPairDecoder : D.Decoder ( String, Int )
tagPairDecoder =
    D.map2 Tuple.pair
        (D.index 0 D.string)
        (D.index 1 D.int)


encodeMemoryInput :
    { title : String
    , timeFrom : String
    , timeTo : String
    , description : String
    , location : String
    , tags : String
    }
    -> E.Value
encodeMemoryInput m =
    let
        tags =
            m.tags
                |> String.split ","
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        nullable s =
            if String.isEmpty s then
                E.null

            else
                E.string s
    in
    E.object
        [ ( "title", E.string m.title )
        , ( "timeFrom", E.string m.timeFrom )
        , ( "timeTo", E.string m.timeTo )
        , ( "description", nullable m.description )
        , ( "location", nullable m.location )
        , ( "tags", E.list E.string tags )
        , ( "photos", E.list E.string [] )
        ]


encodeImportantDayInput : { title : String, date : String, kind : String } -> E.Value
encodeImportantDayInput d =
    E.object
        [ ( "title", E.string d.title )
        , ( "date", E.string d.date )
        , ( "note", E.null )
        , ( "kind", E.string d.kind )
        ]


encodeNoteInput : { owner : String, title : String, body : String } -> E.Value
encodeNoteInput n =
    E.object
        [ ( "owner", E.string n.owner )
        , ( "title", E.string n.title )
        , ( "body", E.string n.body )
        ]


encodePlanInput : { category : String, title : String, detail : String, done : Bool } -> E.Value
encodePlanInput p =
    E.object
        [ ( "category", E.string p.category )
        , ( "title", E.string p.title )
        , ( "detail", if String.isEmpty p.detail then E.null else E.string p.detail )
        , ( "done", E.bool p.done )
        ]


encodeDiaryInput : { date : String, mood : String, body : String } -> E.Value
encodeDiaryInput e =
    E.object
        [ ( "date", E.string e.date )
        , ( "mood", E.string e.mood )
        , ( "body", E.string e.body )
        , ( "weather", E.null )
        ]
