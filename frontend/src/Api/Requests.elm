module Api.Requests exposing
    ( createDiaryReq
    , createImportantDayReq
    , createLocationReq
    , createMemoryReq
    , createNoteReq
    , createPlanCategoryReq
    , createPlanReq
    , createTagReq
    , deleteDiaryReq
    , deleteImportantDayReq
    , deleteLocationReq
    , deleteMemoryReq
    , deleteNoteReq
    , deletePlanCategoryReq
    , deletePlanReq
    , deleteTagReq
    , deleteTrashReq
    , fetchAddressSuggestions
    , fetchDiary
    , fetchFilteredMemories
    , fetchImportantDays
    , fetchLocationCatalog
    , fetchMemories
    , fetchNotes
    , fetchOnThisDay
    , fetchPlanCategories
    , fetchPlans
    , fetchProfile
    , fetchRelationship
    , fetchStats
    , fetchTagCatalog
    , fetchTrash
    , refreshAllData
    , refreshDiary
    , refreshImportantDays
    , refreshMemories
    , refreshNotes
    , refreshPlans
    , renameLocationReq
    , renamePlanCategoryReq
    , renameTagReq
    , restoreTrashReq
    , saveProfile
    , saveRelationship
    , updateDiaryReq
    , updateImportantDayReq
    , updateMemoryReq
    , updateNoteReq
    , updatePlanReq
    , uploadAvatarPhoto
    , uploadPhotos
    )

{-| HTTP requests to the backend REST API.

Every function returns a `Cmd Msg`, responses arrive as `Msg` values in the update function
-}

import Api.Codec
    exposing
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

import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Types exposing (..)
import Url.Builder as UB



-- FETCH


apiPath : List String -> String
apiPath segments =
    UB.absolute ("api" :: segments) []


fetchMemories : Cmd Msg
fetchMemories =
    Http.get { url = apiPath [ "memories" ], expect = Http.expectJson GotMemories (D.list memoryDecoder) }


fetchFilteredMemories : Filter -> Cmd Msg
fetchFilteredMemories f =
    let
        params =
            List.filterMap identity
                [ if String.isEmpty f.q then
                    Nothing

                  else
                    Just (UB.string "q" f.q)
                , if String.isEmpty f.tag then
                    Nothing

                  else
                    Just (UB.string "tag" f.tag)
                , if String.isEmpty f.location then
                    Nothing

                  else
                    Just (UB.string "location" f.location)
                , if String.isEmpty f.dateFrom then
                    Nothing

                  else
                    Just (UB.string "from" f.dateFrom)
                , if String.isEmpty f.dateTo then
                    Nothing

                  else
                    Just (UB.string "to" f.dateTo)
                ]

        url =
            UB.absolute [ "api", "memories" ] params
    in
    Http.get { url = url, expect = Http.expectJson GotTimelineMemories (D.list memoryDecoder) }


fetchOnThisDay : Cmd Msg
fetchOnThisDay =
    Http.get { url = apiPath [ "memories", "on-this-day" ], expect = Http.expectJson GotOnThisDay (D.list memoryDecoder) }


fetchStats : Cmd Msg
fetchStats =
    Http.get { url = apiPath [ "stats" ], expect = Http.expectJson GotStats statsDecoder }


fetchTagCatalog : Cmd Msg
fetchTagCatalog =
    Http.get { url = apiPath [ "tags" ], expect = Http.expectJson GotTagCatalog (D.list D.string) }


fetchLocationCatalog : Cmd Msg
fetchLocationCatalog =
    Http.get { url = apiPath [ "locations" ], expect = Http.expectJson GotLocationCatalog (D.list D.string) }


fetchPlanCategories : Cmd Msg
fetchPlanCategories =
    Http.get { url = apiPath [ "plan-categories" ], expect = Http.expectJson GotPlanCategories (D.list D.string) }


fetchImportantDays : Cmd Msg
fetchImportantDays =
    Http.get { url = apiPath [ "important-days" ], expect = Http.expectJson GotImportantDays (D.list importantDayDecoder) }


fetchRelationship : Cmd Msg
fetchRelationship =
    Http.get { url = apiPath [ "relationship" ], expect = Http.expectJson GotRelationship relationshipDecoder }


fetchProfile : String -> (Result Http.Error ProfileData -> Msg) -> Cmd Msg
fetchProfile side toMsg =
    Http.get { url = apiPath [ "profiles", side ], expect = Http.expectJson toMsg profileDecoder }


saveRelationship : String -> String -> Cmd Msg
saveRelationship startDate heartColor =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "relationship" ]
        , body = Http.jsonBody (encodeRelationshipInput startDate heartColor)
        , expect = Http.expectJson RelationshipSaved relationshipDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveProfile : AvatarProfile -> String -> Cmd Msg
saveProfile avatar side =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "profiles", side ]
        , body = Http.jsonBody (encodeProfileInput avatar)
        , expect = Http.expectJson (ProfileSaved side) profileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchNotes : Cmd Msg
fetchNotes =
    Http.get { url = apiPath [ "notes" ], expect = Http.expectJson GotNotes (D.list noteDecoder) }


fetchPlans : Cmd Msg
fetchPlans =
    Http.get { url = apiPath [ "plans" ], expect = Http.expectJson GotPlans (D.list planDecoder) }


fetchAddressSuggestions : String -> Cmd Msg
fetchAddressSuggestions query =
    if String.length query < 2 then
        Cmd.none

    else
        Http.get
            { url = UB.absolute [ "api", "geocode" ] [ UB.string "q" query ]
            , expect = Http.expectJson (GotAddressSuggestions query) nominatimDecoder
            }


fetchDiary : Cmd Msg
fetchDiary =
    Http.get { url = apiPath [ "diary" ], expect = Http.expectJson GotDiary (D.list diaryEntryDecoder) }

refreshDiary : Cmd Msg
refreshDiary =
    fetchDiary


refreshAllData : Filter -> Cmd Msg
refreshAllData f =
    Cmd.batch
        [ fetchMemories
        , fetchFilteredMemories f
        , fetchOnThisDay
        , fetchStats
        , fetchTagCatalog
        , fetchLocationCatalog
        , fetchPlanCategories
        , fetchImportantDays
        , fetchRelationship
        , fetchProfile "left" GotLeftProfile
        , fetchProfile "right" GotRightProfile
        , fetchNotes
        , fetchPlans
        , fetchDiary
        ]


refreshMemories : Filter -> Cmd Msg
refreshMemories f =
    Cmd.batch
        [ fetchMemories
        , fetchFilteredMemories f
        , fetchOnThisDay
        , fetchStats
        , fetchTagCatalog
        , fetchLocationCatalog
        ]


refreshImportantDays : Cmd Msg
refreshImportantDays =
    fetchImportantDays


refreshNotes : Cmd Msg
refreshNotes =
    fetchNotes


refreshPlans : Cmd Msg
refreshPlans =
    Cmd.batch [ fetchPlans, fetchPlanCategories ]



-- TRASH


fetchTrash : Cmd Msg
fetchTrash =
    Http.get { url = apiPath [ "trash" ], expect = Http.expectJson GotTrash (D.list trashEntryDecoder) }


restoreTrashReq : Int -> Cmd Msg
restoreTrashReq tid =
    Http.post
        { url = apiPath [ "trash", String.fromInt tid, "restore" ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (TrashItemRestored tid)
        }


deleteTrashReq : Int -> Cmd Msg
deleteTrashReq tid =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "trash", String.fromInt tid ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (TrashItemDeleted tid)
        , timeout = Nothing
        , tracker = Nothing
        }



-- MEMORIES


createMemoryReq : MemForm -> Cmd Msg
createMemoryReq form =
    Http.post
        { url = apiPath [ "memories" ]
        , body = Http.jsonBody (encodeMemForm form)
        , expect = Http.expectJson MemoryCreated memoryDecoder
        }


updateMemoryReq : Int -> MemForm -> Cmd Msg
updateMemoryReq id form =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "memories", String.fromInt id ]
        , body = Http.jsonBody (encodeMemForm form)
        , expect = Http.expectWhatever MemoryUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


deleteMemoryReq : Int -> Cmd Msg
deleteMemoryReq id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "memories", String.fromInt id ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (MemoryDeleted id)
        , timeout = Nothing
        , tracker = Nothing
        }



-- ANNIVERSARIES


createImportantDayReq : ImportantDayForm -> Cmd Msg
createImportantDayReq form =
    Http.post
        { url = apiPath [ "important-days" ]
        , body = Http.jsonBody (encodeImportantDayForm form)
        , expect = Http.expectJson ImportantDayCreated importantDayDecoder
        }


updateImportantDayReq : Int -> ImportantDayForm -> Cmd Msg
updateImportantDayReq id form =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "important-days", String.fromInt id ]
        , body = Http.jsonBody (encodeImportantDayForm form)
        , expect = Http.expectWhatever ImportantDayUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


deleteImportantDayReq : Int -> Cmd Msg
deleteImportantDayReq id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "important-days", String.fromInt id ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (ImportantDayDeleted id)
        , timeout = Nothing
        , tracker = Nothing
        }



-- NOTES


createNoteReq : NoteForm -> Cmd Msg
createNoteReq form =
    Http.post
        { url = apiPath [ "notes" ]
        , body = Http.jsonBody (encodeNoteForm form)
        , expect = Http.expectJson NoteCreated noteDecoder
        }


updateNoteReq : Int -> NoteForm -> Cmd Msg
updateNoteReq id form =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "notes", String.fromInt id ]
        , body = Http.jsonBody (encodeNoteForm form)
        , expect = Http.expectWhatever NoteUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


deleteNoteReq : Int -> Cmd Msg
deleteNoteReq id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "notes", String.fromInt id ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (NoteDeleted id)
        , timeout = Nothing
        , tracker = Nothing
        }



-- PLANS


createPlanReq : PlanForm -> Cmd Msg
createPlanReq form =
    Http.post
        { url = apiPath [ "plans" ]
        , body = Http.jsonBody (encodePlanForm form)
        , expect = Http.expectJson PlanCreated planDecoder
        }


updatePlanReq : Int -> PlanForm -> Cmd Msg
updatePlanReq id form =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "plans", String.fromInt id ]
        , body = Http.jsonBody (encodePlanForm form)
        , expect = Http.expectWhatever PlanUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


deletePlanReq : Int -> Cmd Msg
deletePlanReq id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "plans", String.fromInt id ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (PlanDeleted id)
        , timeout = Nothing
        , tracker = Nothing
        }



-- DIARY


createDiaryReq : DiaryForm -> Cmd Msg
createDiaryReq form =
    Http.post
        { url = apiPath [ "diary" ]
        , body = Http.jsonBody (encodeDiaryForm form)
        , expect = Http.expectJson DiaryEntryCreated diaryEntryDecoder
        }


updateDiaryReq : Int -> DiaryForm -> Cmd Msg
updateDiaryReq id form =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "diary", String.fromInt id ]
        , body = Http.jsonBody (encodeDiaryForm form)
        , expect = Http.expectWhatever DiaryEntryUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


deleteDiaryReq : Int -> Cmd Msg
deleteDiaryReq id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "diary", String.fromInt id ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (DiaryEntryDeleted id)
        , timeout = Nothing
        , tracker = Nothing
        }



-- UPLOADS


uploadPhotos : List File -> Cmd Msg
uploadPhotos files =
    Http.post
        { url = apiPath [ "photos" ]
        , body = Http.multipartBody (List.map (Http.filePart "file") files)
        , expect = Http.expectJson PhotosUploaded (D.list D.string)
        }


uploadAvatarPhoto : AvatarSide -> List File -> Cmd Msg
uploadAvatarPhoto side files =
    Http.post
        { url = apiPath [ "photos" ]
        , body = Http.multipartBody (List.map (Http.filePart "file") files)
        , expect = Http.expectJson (AvatarPhotoUploaded side) (D.list D.string)
        }



-- BULK TAG / LOCATION / CATEGORY


createCatalogReq : String -> (Result Http.Error () -> Msg) -> String -> Cmd Msg
createCatalogReq resource toMsg name =
    Http.post
        { url = apiPath [ resource ]
        , body = Http.jsonBody (E.object [ ( "name", E.string name ) ])
        , expect = Http.expectWhatever toMsg
        }


createTagReq : String -> Cmd Msg
createTagReq =
    createCatalogReq "tags" TagCreated


createLocationReq : String -> Cmd Msg
createLocationReq =
    createCatalogReq "locations" LocationCreated


createPlanCategoryReq : String -> Cmd Msg
createPlanCategoryReq =
    createCatalogReq "plan-categories" PlanCategoryCreated


renameTagReq : String -> String -> Cmd Msg
renameTagReq oldName newName =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "tags", oldName, "rename" ]
        , body = Http.jsonBody (E.object [ ( "newName", E.string newName ) ])
        , expect = Http.expectWhatever TagRenamed
        , timeout = Nothing
        , tracker = Nothing
        }


deleteTagReq : String -> Cmd Msg
deleteTagReq name =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "tags", name ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever TagDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


renameLocationReq : String -> String -> Cmd Msg
renameLocationReq oldName newName =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "locations", oldName, "rename" ]
        , body = Http.jsonBody (E.object [ ( "newName", E.string newName ) ])
        , expect = Http.expectWhatever LocationRenamed
        , timeout = Nothing
        , tracker = Nothing
        }


deleteLocationReq : String -> Cmd Msg
deleteLocationReq name =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "locations", name ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever LocationDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


renamePlanCategoryReq : String -> String -> Cmd Msg
renamePlanCategoryReq oldName newName =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiPath [ "plan-categories", oldName, "rename" ]
        , body = Http.jsonBody (E.object [ ( "newName", E.string newName ) ])
        , expect = Http.expectWhatever PlanCategoryRenamed
        , timeout = Nothing
        , tracker = Nothing
        }


deletePlanCategoryReq : String -> String -> Cmd Msg
deletePlanCategoryReq name scope =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiPath [ "plan-categories", name ]
        , body = Http.jsonBody (E.object [ ( "scope", E.string scope ) ])
        , expect = Http.expectWhatever PlanCategoryDeleted
        , timeout = Nothing
        , tracker = Nothing
        }
