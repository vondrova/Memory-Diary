module Update exposing (update)

{-| Root TEA update dispatcher

This layer stays thin: most `Msg` branches delegate to domain-specific update
submodules.

  - `Update.Memories` handles memory CRUD, photos, tags, and geocoding.
  - `Update.ImportantDays` handles manual important days and profile birthdays.
  - `Update.Notes` handles notebook entries.
  - `Update.Plans` handles plans and categories.
  - `Update.Relationship` handles relationship settings and profiles.
  - `Update.Tags` handles bulk tag and place operations.

Navigation and language messages stay here because they depend on `Nav.Key`.

-}

import Api.Requests
    exposing
        ( deleteTrashReq
        , fetchFilteredMemories
        , fetchTrash
        , refreshAllData
        , restoreTrashReq
        )
import Browser
import Browser.Navigation as Nav
import Utils.DateUtils exposing (nextMonth, prevMonth)
import Utils.Helpers exposing (setFFrom, setFLoc, setFQ, setFTag, setFTo)
import Init exposing (emptyFilter)
import Routing exposing (languageFromUrl, pageFromUrl, pageUrlFor)
import Types exposing (..)
import Update.Diary as Diary
import Update.ImportantDays as ImportantDays
import Update.Memories as Memories
import Update.Notes as Notes
import Update.Plans as Plans
import Update.Relationship as Relationship
import Update.Tags as Tags
import Utils.Helpers exposing (mutationFailure)
import Url exposing (Url)


-- | Root TEA update, dispatches every Msg to the appropriate domain handler
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Navigation
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | page = pageFromUrl url, lang = Maybe.withDefault model.lang (languageFromUrl url), selectedDay = Nothing }, Cmd.none )

        GoTo page ->
            ( model, Nav.pushUrl model.key (pageUrlFor model.lang page) )

        ToggleLanguage ->
            let
                nextLang =
                    case model.lang of
                        English ->
                            Czech

                        Czech ->
                            English
            in
            ( { model | lang = nextLang }, Nav.replaceUrl model.key (pageUrlFor nextLang model.page) )

        -- Memory data loading
        GotMemories result ->
            Memories.gotMemories result model

        GotTimelineMemories result ->
            Memories.gotTimelineMemories result model

        GotOnThisDay result ->
            Memories.gotOnThisDay result model

        GotStats result ->
            Memories.gotStats result model

        -- Memory form
        OpenNewMemForm ->
            Memories.openNewForm model

        OpenEditMemForm mem ->
            Memories.openEditForm mem model

        CloseMemForm ->
            Memories.closeForm model

        SetTitle v ->
            Memories.setTitle v model

        SetTimeFrom v ->
            Memories.setTimeFrom v model

        SetTimeTo v ->
            Memories.setTimeTo v model

        SetDescription v ->
            Memories.setDescription v model

        SetLocation v ->
            Memories.setLocation v model

        DoGeocodeIfCurrent seq query ->
            Memories.doGeocodeIfCurrent seq query model

        GotAddressSuggestions q r ->
            Memories.gotAddressSuggestions q r model

        SetTagInput v ->
            Memories.setTagInput v model

        AddTag ->
            Memories.addTag model

        RemoveTag t ->
            Memories.removeTag t model

        SetPhotoInput v ->
            Memories.setPhotoInput v model

        AddPhotoByName ->
            Memories.addPhotoByName model

        SubmitMemForm ->
            Memories.submitMemForm model

        DeleteMemory id ->
            Memories.deleteMemory id model

        PickPhotos ->
            Memories.pickPhotos model

        FilesSelected first rest ->
            Memories.filesSelected first rest model

        PhotosUploaded result ->
            Memories.photosUploaded result model

        RemovePhoto fn ->
            Memories.removePhoto fn model

        AddTagFromCombo tag ->
            Memories.addTagFromCombo tag model

        ToggleLocSuggest ->
            Memories.toggleLocSuggest model

        SelectLocSuggest loc ->
            Memories.selectLocSuggest loc model

        ToggleTagSuggest ->
            Memories.toggleTagSuggest model

        -- Memory CRUD responses
        MemoryCreated result ->
            Memories.memoryCreated result model

        MemoryUpdated result ->
            Memories.memoryUpdated result model

        MemoryDeleted id result ->
            Memories.memoryDeleted id result model

        -- ImportantDay
        GotImportantDays result ->
            ImportantDays.gotImportantDays result model

        ImportantDayCreated result ->
            ImportantDays.importantDayCreated result model

        ImportantDayUpdated result ->
            ImportantDays.importantDayUpdated result model

        ImportantDayDeleted id result ->
            ImportantDays.importantDayDeleted id result model

        OpenNewImportantDayForm ->
            ImportantDays.openNewForm model

        OpenEditImportantDayForm day ->
            ImportantDays.openEditForm day model

        CloseImportantDayForm ->
            ImportantDays.closeForm model

        SetImportantDayTitle v ->
            ImportantDays.setTitle v model

        SetImportantDayDate v ->
            ImportantDays.setDate v model

        SetImportantDayNote v ->
            ImportantDays.setNote v model

        SubmitImportantDayForm ->
            ImportantDays.submitForm model

        DeleteImportantDay id ->
            ImportantDays.deleteImportantDay id model

        -- Notes
        GotNotes result ->
            Notes.gotNotes result model

        NoteCreated result ->
            Notes.noteCreated result model

        NoteUpdated result ->
            Notes.noteUpdated result model

        NoteDeleted id result ->
            Notes.noteDeleted id result model

        OpenNewNoteForm ->
            Notes.openNewForm model

        OpenEditNoteForm note ->
            Notes.openEditForm note model

        CloseNoteForm ->
            Notes.closeForm model

        SetNoteOwner v ->
            Notes.setOwner v model

        SetNoteTitle v ->
            Notes.setTitle v model

        SetNoteBody v ->
            Notes.setBody v model

        SetNoteTab tab ->
            Notes.setTab tab model

        SetNoteSearch v ->
            Notes.setSearch v model

        SubmitNoteForm ->
            Notes.submitForm model

        DeleteNote id ->
            Notes.deleteNote id model

        -- Plans
        GotPlans result ->
            Plans.gotPlans result model

        GotPlanCategories result ->
            Plans.gotPlanCategories result model

        PlanCreated result ->
            Plans.planCreated result model

        PlanUpdated result ->
            Plans.planUpdated result model

        PlanDeleted id result ->
            Plans.planDeleted id result model

        OpenNewPlanForm ->
            Plans.openNewForm model

        OpenEditPlanForm plan ->
            Plans.openEditForm plan model

        ClosePlanForm ->
            Plans.closeForm model

        SetPlanCategory v ->
            Plans.setCategory v model

        TogglePlanCategorySuggest ->
            Plans.toggleCategorySuggest model

        SelectPlanCategorySuggest category ->
            Plans.selectCategorySuggest category model

        SetPlanTitle v ->
            Plans.setTitle v model

        SetPlanDetail v ->
            Plans.setDetail v model

        SetPlanDone v ->
            Plans.setDone v model

        SubmitPlanForm ->
            Plans.submitForm model

        TogglePlanDone plan ->
            Plans.toggleDone plan model

        DeletePlan id ->
            Plans.deletePlan id model

        SetPlanTab tab ->
            Plans.setTab tab model

        OpenCategoryPanel ->
            Plans.openCategoryPanel model

        OpenCategoryPanelEdit oldName ->
            Plans.openCategoryPanelEdit oldName model

        CloseCategoryPanel ->
            Plans.closeCategoryPanel model

        CancelCategoryEdit ->
            Plans.cancelCategoryEdit model

        SetCategoryNewName v ->
            Plans.setCategoryNewName v model

        SetCategoryEditName v ->
            Plans.setCategoryEditName v model

        SubmitNewCategory ->
            Plans.submitNewCategory model

        SubmitCategoryRename ->
            Plans.submitCategoryRename model

        PlanCategoryRenamed result ->
            Plans.planCategoryRenamed result model

        PlanCategoryCreated result ->
            Plans.planCategoryCreated result model

        DeleteCategory name tab ->
            Plans.deleteCategory name tab model

        PlanCategoryDeleted result ->
            Plans.planCategoryDeleted result model

        -- Relationship / Profiles
        GotRelationship result ->
            Relationship.gotRelationship result model

        GotLeftProfile result ->
            Relationship.gotLeftProfile result model

        GotRightProfile result ->
            Relationship.gotRightProfile result model

        RelationshipSaved result ->
            Relationship.relationshipSaved result model

        ProfileSaved side result ->
            Relationship.profileSaved side result model

        SetRelationshipDate v ->
            Relationship.setDate v model

        SetHeartColor v ->
            Relationship.setHeartColor v model

        OpenHeartEditor ->
            Relationship.openHeartEditor model

        CloseHeartEditor ->
            Relationship.closeHeartEditor model

        SubmitRelationship ->
            Relationship.submitRelationship model

        OpenRelationshipEditor ->
            Relationship.openRelationshipEditor model

        CloseRelationshipEditor ->
            Relationship.closeRelationshipEditor model

        SetAvatarName side v ->
            Relationship.setAvatarName side v model

        SetAvatarMode side v ->
            Relationship.setAvatarMode side v model

        SetAvatarPhoto side v ->
            Relationship.setAvatarPhoto side v model

        SetAvatarFigureColor side v ->
            Relationship.setAvatarFigureColor side v model

        SetAvatarAccessory side v ->
            Relationship.setAvatarAccessory side v model

        SetAvatarExpression side v ->
            Relationship.setAvatarExpression side v model

        SetAvatarBirthday side v ->
            Relationship.setAvatarBirthday side v model

        PickAvatarPhoto side ->
            Relationship.pickAvatarPhoto side model

        AvatarPhotoSelected side f rest ->
            Relationship.avatarPhotoSelected side f rest model

        AvatarPhotoUploaded side result ->
            Relationship.avatarPhotoUploaded side result model

        OpenAvatarEditor side ->
            Relationship.openAvatarEditor side model

        CloseAvatarEditor ->
            Relationship.closeAvatarEditor model

        SubmitAvatar side ->
            Relationship.submitAvatar side model

        -- Tags / Locations
        GotTagCatalog result ->
            Tags.gotTagCatalog result model

        GotLocationCatalog result ->
            Tags.gotLocationCatalog result model

        OpenTagPanel ->
            Tags.openTagPanel model

        CloseTagPanel ->
            Tags.closeTagPanel model

        StartEditTag name ->
            Tags.startEditTag name model

        CancelEditTag ->
            Tags.cancelEditTag model

        SetTagEditName s ->
            Tags.setTagEditName s model

        SubmitTagRename ->
            Tags.submitTagRename model

        TagRenamed result ->
            Tags.tagRenamed result model

        DeleteTagFromAll tag ->
            Tags.deleteTagFromAll tag model

        TagDeleted result ->
            Tags.tagDeleted result model

        TagCreated result ->
            Tags.tagCreated result model

        SetTagNewName s ->
            Tags.setTagNewName s model

        SubmitNewTag ->
            Tags.submitNewTag model

        OpenLocationPanel ->
            Tags.openLocationPanel model

        CloseLocationPanel ->
            Tags.closeLocationPanel model

        StartEditLocation name ->
            Tags.startEditLocation name model

        CancelEditLocation ->
            Tags.cancelEditLocation model

        SetLocationEditName s ->
            Tags.setLocationEditName s model

        SubmitLocationRename ->
            Tags.submitLocationRename model

        LocationRenamed result ->
            Tags.locationRenamed result model

        DeleteLocationFromAll loc ->
            Tags.deleteLocationFromAll loc model

        LocationDeleted result ->
            Tags.locationDeleted result model

        LocationCreated result ->
            Tags.locationCreated result model

        SetLocationNewName s ->
            Tags.setLocationNewName s model

        SubmitNewLocation ->
            Tags.submitNewLocation model

        -- Filters
        SetFilterQ v ->
            let
                f =
                    setFQ v model.filter
            in
            ( { model | filter = f }, fetchFilteredMemories f )

        SetFilterTag v ->
            let
                f =
                    setFTag v model.filter
            in
            ( { model | filter = f }, fetchFilteredMemories f )

        SetFilterLocation v ->
            let
                f =
                    setFLoc v model.filter
            in
            ( { model | filter = f }, fetchFilteredMemories f )

        SetFilterFrom v ->
            let
                f =
                    setFFrom v model.filter
            in
            ( { model | filter = f }, fetchFilteredMemories f )

        SetFilterTo v ->
            let
                f =
                    setFTo v model.filter
            in
            ( { model | filter = f }, fetchFilteredMemories f )

        ClearFilters ->
            ( { model | filter = emptyFilter, timelineMemories = model.memories }, fetchFilteredMemories emptyFilter )

        -- Calendar
        PrevMonth ->
            let
                ( y, m ) =
                    prevMonth model.calYear model.calMonth
            in
            ( { model | calYear = y, calMonth = m, selectedDay = Nothing }, Cmd.none )

        NextMonth ->
            let
                ( y, m ) =
                    nextMonth model.calYear model.calMonth
            in
            ( { model | calYear = y, calMonth = m, selectedDay = Nothing }, Cmd.none )

        SelectDay d ->
            ( { model
                | selectedDay =
                    if model.selectedDay == Just d then
                        Nothing

                    else
                        Just d
              }
            , Cmd.none
            )

        -- Confirm dialog
        RequestConfirm action message ->
            ( { model | confirmDialog = Just { message = message, action = action } }, Cmd.none )

        CancelConfirm ->
            ( { model | confirmDialog = Nothing }, Cmd.none )

        CloseInfoDialog ->
            ( { model | infoDialog = Nothing }, Cmd.none )

        DoConfirm ->
            case model.confirmDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just dialog ->
                    let
                        newModel =
                            { model | confirmDialog = Nothing }
                    in
                    case dialog.action of
                        ConfirmDeleteMemory id ->
                            Memories.deleteMemory id newModel

                        ConfirmDeleteImportantDay id ->
                            ImportantDays.deleteImportantDay id newModel

                        ConfirmDeleteNote id ->
                            Notes.deleteNote id newModel

                        ConfirmDeletePlan id ->
                            Plans.deletePlan id newModel

                        ConfirmDeleteDiaryEntry id ->
                            Diary.deleteDiaryEntry id newModel

                        ConfirmDeleteCategory name tab ->
                            Plans.deleteCategory name tab newModel

                        ConfirmDeleteTagFromAll tag ->
                            Tags.deleteTagFromAll tag newModel

                        ConfirmDeleteLocationFromAll loc ->
                            Tags.deleteLocationFromAll loc newModel

        -- Trash
        OpenTrashPanel ->
            ( { model | trashPanelOpen = True, trash = Loading }, fetchTrash )

        CloseTrashPanel ->
            ( { model | trashPanelOpen = False }, Cmd.none )

        GotTrash result ->
            case result of
                Ok entries ->
                    ( { model | trash = Loaded entries }, Cmd.none )

                Err _ ->
                    ( { model | trash = Failed "Could not load trash" }, Cmd.none )

        RestoreTrashItem tid ->
            ( model, restoreTrashReq tid )

        TrashItemRestored tid result ->
            case result of
                Ok _ ->
                    let
                        newTrash =
                            case model.trash of
                                Loaded entries ->
                                    Loaded (List.filter (\e -> e.id /= tid) entries)

                                other ->
                                    other
                    in
                    ( { model | trash = newTrash }, refreshAllData model.filter )

                Err error ->
                    ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )

        DeleteTrashItemPermanently tid ->
            ( model, deleteTrashReq tid )

        TrashItemDeleted tid result ->
            case result of
                Ok _ ->
                    let
                        newTrash =
                            case model.trash of
                                Loaded entries ->
                                    Loaded (List.filter (\e -> e.id /= tid) entries)

                                other ->
                                    other
                    in
                    ( { model | trash = newTrash }, Cmd.none )

                Err error ->
                    ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )

        SetThemeColor color ->
            ( { model | themeColor = color, themePickerOpen = False }, Cmd.none )

        OpenThemePicker ->
            ( { model | themePickerOpen = True }, Cmd.none )

        CloseThemePicker ->
            ( { model | themePickerOpen = False }, Cmd.none )

        -- Diary
        GotDiary result ->
            Diary.gotDiary result model

        DiaryEntryCreated result ->
            Diary.diaryEntryCreated result model

        DiaryEntryUpdated result ->
            Diary.diaryEntryUpdated result model

        DiaryEntryDeleted id result ->
            Diary.diaryEntryDeleted id result model

        OpenNewDiaryEntry ->
            Diary.openNewDiaryEntry model

        OpenEditDiaryEntry entry ->
            Diary.openEditDiaryEntry entry model

        CloseDiaryForm ->
            Diary.closeDiaryForm model

        SetDiaryDate v ->
            Diary.setDiaryDate v model

        SetDiaryBody v ->
            Diary.setDiaryBody v model

        SubmitDiaryForm ->
            Diary.submitDiaryForm model

        DeleteDiaryEntry id ->
            Diary.deleteDiaryEntry id model

        SetDiaryTab tab ->
            Diary.setDiaryTab tab model

        NoOp ->
            ( model, Cmd.none )
