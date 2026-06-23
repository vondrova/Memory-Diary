module Init exposing
    ( emptyCategoryForm
    , emptyDiaryForm
    , emptyFilter
    , emptyImportantDayForm
    , emptyMemForm
    , emptyNoteForm
    , emptyPlanForm
    , init
    )

{-| Initial application state and empty form values
-}

import Api.Requests exposing (refreshAllData)
import Browser.Navigation as Nav
import Routing exposing (languageFromUrl, pageFromUrl)
import Types exposing (..)
import Url exposing (Url)



-- EMPTY FORMS -----------------------------------------------
-- | Blank filter with all search fields cleared


emptyFilter : Filter
emptyFilter =
    { q = "", tag = "", location = "", dateFrom = "", dateTo = "" }



-- | Blank memory creation/edit form


emptyMemForm : MemForm
emptyMemForm =
    { open = False, editId = Nothing, title = "", timeFrom = "", timeTo = "", description = "", location = "", locSuggestOpen = False, tagSuggestOpen = False, tags = [], photos = [], showValidation = False }



-- | Blank important-day form


emptyImportantDayForm : ImportantDayForm
emptyImportantDayForm =
    { open = False, editId = Nothing, title = "", date = "", note = "", showValidation = False }



-- | Blank note form; defaults owner to "shared"


emptyNoteForm : NoteForm
emptyNoteForm =
    { open = False, editId = Nothing, owner = "shared", title = "", body = "", showValidation = False }



-- | Blank plan form


emptyPlanForm : PlanForm
emptyPlanForm =
    { open = False, editId = Nothing, category = "", catSuggestOpen = False, title = "", detail = "", done = False, showValidation = False }



-- | Blank category management form


emptyCategoryForm : CategoryForm
emptyCategoryForm =
    { open = False, editOldName = Nothing, editNewName = "", newName = "" }



-- | Blank diary entry form; defaults owner to "left"


emptyDiaryForm : DiaryForm
emptyDiaryForm =
    { open = False, editId = Nothing, owner = "left", date = "", body = "", showValidation = False }



-- DEFAULT AVATARS -----------------------------------------------
-- | Placeholder left avatar shown until a profile loads from the backend


defaultLeftAvatar : AvatarProfile
defaultLeftAvatar =
    { name = "", mode = "figure", photo = "", figureColor = "figure-red", accessory = "flower", expression = "smile", birthday = "" }



-- | Placeholder right avatar shown until a profile loads from the backend


defaultRightAvatar : AvatarProfile
defaultRightAvatar =
    { name = "", mode = "figure", photo = "", figureColor = "figure-blue", accessory = "hat", expression = "laugh", birthday = "" }



-- INIT -----------------------------------------------
-- | Bootstrap the application; `flags` carries today's date from JS, the diary form is pre-filled with today's date


init : { year : Int, month : Int, day : Int } -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialLang =
            Maybe.withDefault English (languageFromUrl url)
    in
    ( { key = key
      , page = pageFromUrl url
      , lang = initialLang
      , today = flags
      , memories = Loading
      , timelineMemories = Loading
      , onThisDay = Loading
      , stats = Loading
      , importantDays = Loading
      , relationship = Loading
      , leftProfile = Loading
      , rightProfile = Loading
      , notes = Loading
      , plans = Loading
      , filter = emptyFilter
      , tagInput = ""
      , photoInput = ""
      , addressSuggestions = []
      , geocodeSeq = 0
      , relationshipDate = ""
      , relationshipValidation = False
      , heartColor = "heart-red"
      , leftAvatar = defaultLeftAvatar
      , rightAvatar = defaultRightAvatar
      , editingAvatar = Nothing
      , editingHeart = False
      , memForm = emptyMemForm
      , importantDayForm = emptyImportantDayForm
      , noteForm = emptyNoteForm
      , noteTab = SharedNotes
      , noteSearch = ""
      , planForm = emptyPlanForm
      , planTab = PendingPlans
      , categoryForm = emptyCategoryForm
      , planCategories = []
      , calYear = flags.year
      , calMonth = flags.month
      , selectedDay = Nothing
      , confirmDialog = Nothing
      , infoDialog = Nothing
      , tagForm = { open = False, editOldName = Nothing, editNewName = "", newName = "" }
      , tagCatalog = []
      , locationForm = { open = False, editOldName = Nothing, editNewName = "", newName = "" }
      , locationCatalog = []
      , trashPanelOpen = False
      , trash = Loaded []
      , themeColor = "theme-rose"
      , themePickerOpen = False
      , editingRelationship = False
      , draftAvatar = Nothing
      , diaryEntries = Loading
      , diaryTab = LeftDiary
      , diaryForm =
            { emptyDiaryForm
                | date =
                    String.fromInt flags.year
                        ++ "-"
                        ++ String.padLeft 2 '0' (String.fromInt flags.month)
                        ++ "-"
                        ++ String.padLeft 2 '0' (String.fromInt flags.day)
                , open = True
            }
      }
    , refreshAllData emptyFilter
    )
