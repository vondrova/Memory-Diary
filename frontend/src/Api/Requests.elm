{- Parameterised HTTP request helpers.
   Every function takes a (Result Http.Error a -> msg) callback
   so this module can be imported from Main without a circular dependency.
-}
module Api.Requests exposing
    ( fetchMemories
    , fetchImportantDays
    , fetchNotes
    , fetchPlans
    , fetchDiary
    , fetchStats
    , searchGeo
    , createMemory
    , createImportantDay
    , createNote
    , createPlan
    , updatePlan
    , createDiaryEntry
    )

import Api.Codec as Codec
import Http
import Json.Decode as D
import Json.Encode as E
import Types exposing (..)


fetchMemories : String -> (Result Http.Error (List Memory) -> msg) -> Cmd msg
fetchMemories q toMsg =
    let
        url =
            if String.isEmpty q then
                "/api/memories"

            else
                "/api/memories?q=" ++ q
    in
    Http.get { url = url, expect = Http.expectJson toMsg (D.list Codec.memoryDecoder) }


fetchImportantDays : (Result Http.Error (List ImportantDay) -> msg) -> Cmd msg
fetchImportantDays toMsg =
    Http.get { url = "/api/important-days", expect = Http.expectJson toMsg (D.list Codec.importantDayDecoder) }


fetchNotes : (Result Http.Error (List CoupleNote) -> msg) -> Cmd msg
fetchNotes toMsg =
    Http.get { url = "/api/notes", expect = Http.expectJson toMsg (D.list Codec.coupleNoteDecoder) }


fetchPlans : (Result Http.Error (List CouplePlan) -> msg) -> Cmd msg
fetchPlans toMsg =
    Http.get { url = "/api/plans", expect = Http.expectJson toMsg (D.list Codec.couplePlanDecoder) }


fetchDiary : (Result Http.Error (List DiaryEntry) -> msg) -> Cmd msg
fetchDiary toMsg =
    Http.get { url = "/api/diary", expect = Http.expectJson toMsg (D.list Codec.diaryDecoder) }


fetchStats : (Result Http.Error Stats -> msg) -> Cmd msg
fetchStats toMsg =
    Http.get { url = "/api/stats", expect = Http.expectJson toMsg Codec.statsDecoder }


searchGeo : String -> (Result Http.Error (List GeoSuggestion) -> msg) -> Cmd msg
searchGeo q toMsg =
    Http.get { url = "/api/geocode?q=" ++ q, expect = Http.expectJson toMsg (D.list Codec.geoDecoder) }


createMemory :
    { title : String, timeFrom : String, timeTo : String, description : String, location : String, tags : String }
    -> (Result Http.Error Memory -> msg)
    -> Cmd msg
createMemory fields toMsg =
    Http.post
        { url = "/api/memories"
        , body = Http.jsonBody (Codec.encodeMemoryInput fields)
        , expect = Http.expectJson toMsg Codec.memoryDecoder
        }


createImportantDay : { title : String, date : String, kind : String } -> (Result Http.Error ImportantDay -> msg) -> Cmd msg
createImportantDay fields toMsg =
    Http.post
        { url = "/api/important-days"
        , body = Http.jsonBody (Codec.encodeImportantDayInput fields)
        , expect = Http.expectJson toMsg Codec.importantDayDecoder
        }


createNote : { owner : String, title : String, body : String } -> (Result Http.Error CoupleNote -> msg) -> Cmd msg
createNote fields toMsg =
    Http.post
        { url = "/api/notes"
        , body = Http.jsonBody (Codec.encodeNoteInput fields)
        , expect = Http.expectJson toMsg Codec.coupleNoteDecoder
        }


createPlan : { category : String, title : String, detail : String } -> (Result Http.Error CouplePlan -> msg) -> Cmd msg
createPlan fields toMsg =
    Http.post
        { url = "/api/plans"
        , body = Http.jsonBody (Codec.encodePlanInput { category = fields.category, title = fields.title, detail = fields.detail, done = False })
        , expect = Http.expectJson toMsg Codec.couplePlanDecoder
        }


updatePlan : CouplePlan -> (Result Http.Error CouplePlan -> msg) -> Cmd msg
updatePlan plan toMsg =
    case plan.id of
        Nothing ->
            Cmd.none

        Just planId ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/plans/" ++ String.fromInt planId
                , body = Http.jsonBody (Codec.encodePlanInput { category = plan.category, title = plan.title, detail = Maybe.withDefault "" plan.detail, done = plan.done })
                , expect = Http.expectJson toMsg Codec.couplePlanDecoder
                , timeout = Nothing
                , tracker = Nothing
                }


createDiaryEntry : { date : String, mood : String, body : String } -> (Result Http.Error DiaryEntry -> msg) -> Cmd msg
createDiaryEntry fields toMsg =
    Http.post
        { url = "/api/diary"
        , body = Http.jsonBody (Codec.encodeDiaryInput fields)
        , expect = Http.expectJson toMsg Codec.diaryDecoder
        }
