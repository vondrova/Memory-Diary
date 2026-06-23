{- Root TEA update dispatcher. Delegates each Msg group to its domain sub-module. -}
module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Routing exposing (fromUrl)
import Types exposing (..)
import Update.Diary as Diary
import Update.ImportantDays as ImportantDays
import Update.Memories as Memories
import Update.Notes as Notes
import Update.Plans as Plans
import Update.Relationship as Relationship
import Url exposing (Url)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            ( { model | page = fromUrl url }, Cmd.none )

        ToggleLang ->
            ( { model | lang = if model.lang == CZ then EN else CZ }, Cmd.none )

        ToggleTheme ->
            ( { model | theme = if model.theme == Light then Dark else Light }, Cmd.none )

        GotMemories _  -> Memories.updateMemories msg model
        SetSearch _    -> Memories.updateMemories msg model
        OpenForm       -> Memories.updateMemories msg model
        CloseForm      -> Memories.updateMemories msg model
        SetTitle _     -> Memories.updateMemories msg model
        SetTimeFrom _  -> Memories.updateMemories msg model
        SetTimeTo _    -> Memories.updateMemories msg model
        SetDescription _ -> Memories.updateMemories msg model
        SetLocation _  -> Memories.updateMemories msg model
        SetTags _      -> Memories.updateMemories msg model
        PickPhoto      -> Memories.updateMemories msg model
        PhotoPicked _  -> Memories.updateMemories msg model
        SetGeoQuery _  -> Memories.updateMemories msg model
        SearchGeo      -> Memories.updateMemories msg model
        GotGeoSuggestions _ -> Memories.updateMemories msg model
        SelectGeoSuggestion _ -> Memories.updateMemories msg model
        SubmitForm     -> Memories.updateMemories msg model
        MemoryCreated _ -> Memories.updateMemories msg model
        DeleteMemory _ -> Memories.updateMemories msg model
        RestoreMemory _ -> Memories.updateMemories msg model

        GotImportantDays _ -> ImportantDays.updateImportantDays msg model
        SetNewDayTitle _   -> ImportantDays.updateImportantDays msg model
        SetNewDayDate _    -> ImportantDays.updateImportantDays msg model
        SetNewDayKind _    -> ImportantDays.updateImportantDays msg model
        SubmitNewDay       -> ImportantDays.updateImportantDays msg model
        DayCreated _       -> ImportantDays.updateImportantDays msg model

        GotNotes _    -> Notes.updateNotes msg model
        SetNoteOwner _ -> Notes.updateNotes msg model
        SetNoteTitle _ -> Notes.updateNotes msg model
        SetNoteBody _  -> Notes.updateNotes msg model
        SubmitNote     -> Notes.updateNotes msg model
        NoteCreated _  -> Notes.updateNotes msg model

        GotPlans _      -> Plans.updatePlans msg model
        SetPlanCategory _ -> Plans.updatePlans msg model
        SetPlanTitle _   -> Plans.updatePlans msg model
        SetPlanDetail _  -> Plans.updatePlans msg model
        SubmitPlan       -> Plans.updatePlans msg model
        PlanCreated _    -> Plans.updatePlans msg model
        TogglePlanDone _ -> Plans.updatePlans msg model
        PlanUpdated _    -> Plans.updatePlans msg model

        GotDiary _        -> Diary.updateDiary msg model
        SetDiaryDate _    -> Diary.updateDiary msg model
        SetDiaryBody _    -> Diary.updateDiary msg model
        SetDiaryMood _    -> Diary.updateDiary msg model
        SubmitDiaryEntry  -> Diary.updateDiary msg model
        DiaryCreated _    -> Diary.updateDiary msg model

        GotStats _ -> Relationship.updateRelationship msg model
