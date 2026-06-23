module Api.Codec exposing
    ( diaryEntryDecoder
    , encodeDiaryForm
    , encodeImportantDayForm
    , encodeMemForm
    , encodeNoteForm
    , encodePlanForm
    , encodeProfileInput
    , encodeRelationshipInput
    , importantDayDecoder
    , memoryDecoder
    , nominatimDecoder
    , noteDecoder
    , planDecoder
    , profileDecoder
    , relationshipDecoder
    , statsDecoder
    , trashEntryDecoder
    )

{-| JSON codec for the backend REST API — decoders and encoders in one module

Decoders mirror the JSON structures returned by `backend/src/Types.hs`
Encoders produce the request bodies expected by the same DTOs
-}

import Json.Decode as D
import Json.Encode as E
import Types exposing (..)


-- DECODERS -----------------------------------------------

memoryDecoder : D.Decoder Memory
memoryDecoder =
    D.map8 Memory
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "timeFrom" D.string)
        (D.field "timeTo" D.string)
        (D.field "description" (D.maybe D.string))
        (D.field "location" (D.maybe D.string))
        (D.field "tags" (D.list D.string))
        (D.field "photos" (D.list D.string))


importantDayDecoder : D.Decoder ImportantDay
importantDayDecoder =
    D.succeed ImportantDay
        |> andMap (D.field "id" (D.maybe D.int))
        |> andMap (D.field "title" D.string)
        |> andMap (D.field "date" D.string)
        |> andMap (D.field "note" (D.maybe D.string))
        |> andMap (D.field "kind" D.string)
        |> andMap (D.field "owner" (D.maybe D.string))
        |> andMap (D.field "nextOccurrence" D.string)
        |> andMap (D.field "daysUntil" D.int)
        |> andMap (D.field "month" D.int)
        |> andMap (D.field "day" D.int)


noteDecoder : D.Decoder CoupleNote
noteDecoder =
    D.map4 CoupleNote
        (D.field "id" D.int)
        (D.field "owner" D.string)
        (D.field "title" D.string)
        (D.field "body" D.string)


planDecoder : D.Decoder CouplePlan
planDecoder =
    D.map5 CouplePlan
        (D.field "id" D.int)
        (D.field "category" D.string)
        (D.field "title" D.string)
        (D.field "detail" (D.maybe D.string))
        (D.field "done" D.bool)


pairDecoder : D.Decoder ( String, Int )
pairDecoder =
    D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.int)


statsDecoder : D.Decoder Stats
statsDecoder =
    D.succeed Stats
        |> andMap (D.field "totalMinutes" D.int)
        |> andMap (D.field "memoryCount" D.int)
        |> andMap (D.field "photoCount" D.int)
        |> andMap (D.field "visitedPlaces" D.int)
        |> andMap (D.field "averageMinutes" D.int)
        |> andMap (D.field "longestTitle" (D.maybe D.string))
        |> andMap (D.field "longestMinutes" D.int)
        |> andMap (D.field "topTags" (D.list pairDecoder))
        |> andMap (D.field "topLocations" (D.list pairDecoder))
        |> andMap (D.field "monthlyActivity" (D.list pairDecoder))
        |> andMap (D.field "dailyActivity" (D.list pairDecoder))


-- get one decoder and then map it with a function that takes the result of the decoder and returns a new value (for chaining decoders - more then D.map8 is needed...help from AI)
andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap da df =
    D.andThen (\f -> D.map f da) df


relationshipDecoder : D.Decoder RelationshipData
relationshipDecoder =
    D.succeed RelationshipData
        |> andMap (D.field "startDate" (D.maybe D.string))
        |> andMap (D.field "heartColor" D.string)
        |> andMap (D.field "daysTogether" (D.maybe D.int))
        |> andMap (D.field "nextAnniversary" (D.maybe D.string))
        |> andMap (D.field "anniversaryNumber" (D.maybe D.int))
        |> andMap (D.field "nextMonthiversary" (D.maybe D.string))
        |> andMap (D.field "monthiversaryNum" (D.maybe D.int))
        |> andMap (D.field "anniversaryDays" (D.maybe D.int))
        |> andMap (D.field "monthiversaryDays" (D.maybe D.int))


profileDecoder : D.Decoder ProfileData
profileDecoder =
    D.succeed ProfileData
        |> andMap (D.field "side" D.string)
        |> andMap (D.field "name" D.string)
        |> andMap (D.field "displayMode" D.string)
        |> andMap (D.field "photo" (D.maybe D.string))
        |> andMap (D.field "figureColor" D.string)
        |> andMap (D.field "accessory" D.string)
        |> andMap (D.field "expression" D.string)
        |> andMap (D.field "birthday" (D.maybe D.string))


trashEntryDecoder : D.Decoder TrashEntry
trashEntryDecoder =
    D.map4 TrashEntry
        (D.field "id" D.int)
        (D.field "kind" D.string)
        (D.field "name" D.string)
        (D.field "deletedAt" D.string)


diaryEntryDecoder : D.Decoder DiaryEntry
diaryEntryDecoder =
    D.map4 DiaryEntry
        (D.field "id" D.int)
        (D.field "owner" D.string)
        (D.field "date" D.string)
        (D.field "body" D.string)

-- geocoding proxy
nominatimDecoder : D.Decoder (List String)
nominatimDecoder =
    D.list nominatimResultDecoder


nominatimResultDecoder : D.Decoder String
nominatimResultDecoder =
    D.field "address"
        (D.map3
            (\mRoad mHouse mCity ->
                let
                    road =
                        Maybe.withDefault "" mRoad

                    city =
                        Maybe.withDefault "" mCity

                    base =
                        case mHouse of
                            Just h ->
                                road ++ " " ++ h

                            Nothing ->
                                road
                in
                if String.isEmpty base then
                    city

                else if not (String.isEmpty city) && city /= base then
                    base ++ ", " ++ city

                else
                    base
            )
            (D.maybe (D.field "road" D.string))
            (D.maybe (D.field "house_number" D.string))
            (D.maybe (D.field "city" D.string))
        )


-- ENCODERS -----------------------------------------------


{-| Blank or whitespace-only strings become JSON null, other strings are kept
-}
maybeStr : String -> E.Value
maybeStr s =
    if String.isEmpty (String.trim s) then
        E.null

    else
        E.string s


toUtcString : String -> String
toUtcString s =
    if String.contains "T" s then
        if String.endsWith "Z" s then
            s

        else
            s ++ ":00Z"

    else
        s


encodeMemForm : MemForm -> E.Value
encodeMemForm form =
    E.object
        [ ( "title", E.string form.title )
        , ( "timeFrom", E.string (toUtcString form.timeFrom) )
        , ( "timeTo", E.string (toUtcString form.timeTo) )
        , ( "description", maybeStr form.description )
        , ( "location", maybeStr form.location )
        , ( "tags", E.list E.string form.tags )
        , ( "photos", E.list E.string form.photos )
        ]


encodeImportantDayForm : ImportantDayForm -> E.Value
encodeImportantDayForm form =
    E.object
        [ ( "title", E.string form.title )
        , ( "date", E.string (form.date ++ "T00:00:00Z") )
        , ( "note", maybeStr form.note )
        ]


encodeNoteForm : NoteForm -> E.Value
encodeNoteForm form =
    E.object
        [ ( "owner", E.string form.owner )
        , ( "title", E.string form.title )
        , ( "body", E.string form.body )
        ]


encodePlanForm : PlanForm -> E.Value
encodePlanForm form =
    E.object
        [ ( "category", E.string form.category )
        , ( "title", E.string form.title )
        , ( "detail", maybeStr form.detail )
        , ( "done", E.bool form.done )
        ]


encodeRelationshipInput : String -> String -> E.Value
encodeRelationshipInput startDate heartColor =
    let
        dateFields =
            if String.isEmpty (String.trim startDate) then
                []

            else
                [ ( "startDate", E.string (startDate ++ "T00:00:00Z") ) ]
    in
    E.object (dateFields ++ [ ( "heartColor", E.string heartColor ) ])

encodeDiaryForm : DiaryForm -> E.Value
encodeDiaryForm form =
    E.object
        [ ( "owner", E.string form.owner )
        , ( "date", E.string (form.date ++ "T00:00:00Z") )
        , ( "body", E.string form.body )
        ]


encodeProfileInput : AvatarProfile -> E.Value
encodeProfileInput avatar =
    E.object
        [ ( "name", E.string avatar.name )
        , ( "displayMode", E.string avatar.mode )
        , ( "photo"
          , if String.isEmpty avatar.photo then
                E.null

            else
                E.string avatar.photo
          )
        , ( "figureColor", E.string avatar.figureColor )
        , ( "accessory", E.string avatar.accessory )
        , ( "expression", E.string avatar.expression )
        , ( "birthday"
          , if String.isEmpty (String.trim avatar.birthday) then
                E.null

            else
                E.string (avatar.birthday ++ "T00:00:00Z")
          )
        ]
