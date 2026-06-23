module Types exposing (..)

{-| All shared types for the Mem-Diary frontend
-}

import Browser
import Browser.Navigation as Nav
import File exposing (File)
import Http
import Url exposing (Url)



-- PAGES & NAVIGATION -----------------------------------------------

type Page
    = HomePage
    | TimelinePage
    | CalendarPage
    | StatsPage
    | ImportantDaysPage
    | NotesPage
    | PlansPage
    | DiaryPage


type alias Today =
    { year : Int, month : Int, day : Int }


-- DOMAIN TYPES -----------------------------------------------

type alias Memory =
    { id : Int
    , title : String
    , timeFrom : String
    , timeTo : String
    , description : Maybe String
    , location : Maybe String
    , tags : List String
    , photos : List String
    }


type alias ImportantDay =
    { id : Maybe Int
    , title : String
    , date : String
    , note : Maybe String
    , kind : String
    , owner : Maybe String
    , nextOccurrence : String
    , daysUntil : Int
    , month : Int
    , day : Int
    }


type alias CoupleNote =
    { id : Int
    , owner : String
    , title : String
    , body : String
    }


type alias CouplePlan =
    { id : Int
    , category : String
    , title : String
    , detail : Maybe String
    , done : Bool
    }


type alias Stats =
    { totalMinutes : Int
    , memoryCount : Int
    , photoCount : Int
    , visitedPlaces : Int
    , averageMinutes : Int
    , longestTitle : Maybe String
    , longestMinutes : Int
    , topTags : List ( String, Int )
    , topLocations : List ( String, Int )
    , monthlyActivity : List ( String, Int )
    , dailyActivity : List ( String, Int )
    }


-- | Three-state async wrapper used for every remote data field in the model
type Status a
    = Loading
    | Loaded a
    | Failed String


-- FORM TYPES -----------------------------------------------

type alias Filter =
    { q : String
    , tag : String
    , location : String
    , dateFrom : String
    , dateTo : String
    }


type alias MemForm =
    { open : Bool
    , editId : Maybe Int
    , title : String
    , timeFrom : String
    , timeTo : String
    , description : String
    , location : String
    , locSuggestOpen : Bool
    , tagSuggestOpen : Bool
    , tags : List String
    , photos : List String
    , showValidation : Bool
    }


type alias ImportantDayForm =
    { open : Bool
    , editId : Maybe Int
    , title : String
    , date : String
    , note : String
    , showValidation : Bool
    }


type alias NoteForm =
    { open : Bool
    , editId : Maybe Int
    , owner : String
    , title : String
    , body : String
    , showValidation : Bool
    }


type alias PlanForm =
    { open : Bool
    , editId : Maybe Int
    , category : String
    , catSuggestOpen : Bool
    , title : String
    , detail : String
    , done : Bool
    , showValidation : Bool
    }


type alias CategoryForm =
    { open : Bool
    , editOldName : Maybe String
    , editNewName : String
    , newName : String
    }


-- UI STATE TYPES -----------------------------------------------

type AvatarSide
    = LeftAvatar
    | RightAvatar


type PlanTab
    = PendingPlans
    | DonePlans


type NoteTab
    = SharedNotes
    | LeftAvatarNotes
    | RightAvatarNotes


type Language
    = English
    | Czech


type ConfirmAction
    = ConfirmDeleteMemory Int
    | ConfirmDeleteImportantDay Int
    | ConfirmDeleteNote Int
    | ConfirmDeletePlan Int
    | ConfirmDeleteDiaryEntry Int
    | ConfirmDeleteCategory String PlanTab
    | ConfirmDeleteTagFromAll String
    | ConfirmDeleteLocationFromAll String


type alias TagLocationForm =
    { open : Bool
    , editOldName : Maybe String
    , editNewName : String
    , newName : String
    }


type alias TrashEntry =
    { id : Int
    , kind : String
    , name : String
    , deletedAt : String
    }


type alias AvatarProfile =
    { name : String
    , mode : String
    , photo : String
    , figureColor : String
    , accessory : String
    , expression : String
    , birthday : String
    }


type alias RelationshipData =
    { startDate : Maybe String
    , heartColor : String
    , daysTogether : Maybe Int
    , nextAnniversary : Maybe String
    , anniversaryNumber : Maybe Int
    , nextMonthiversary : Maybe String
    , monthiversaryNum : Maybe Int
    , anniversaryDays : Maybe Int
    , monthiversaryDays : Maybe Int
    }


type alias ProfileData =
    { side : String
    , name : String
    , displayMode : String
    , photo : Maybe String
    , figureColor : String
    , accessory : String
    , expression : String
    , birthday : Maybe String
    }


type alias UpcomingEvent =
    { title : String
    , date : Today
    , days : Int
    , kind : String
    }


type DiaryTab
    = LeftDiary
    | RightDiary


type alias DiaryEntry =
    { id : Int
    , owner : String
    , date : String
    , body : String
    }


type alias DiaryForm =
    { open : Bool
    , editId : Maybe Int
    , owner : String
    , date : String
    , body : String
    , showValidation : Bool
    }


-- MODEL -----------------------------------------------

type alias Model =
    { key : Nav.Key
    , page : Page
    , lang : Language
    , today : Today
    , memories : Status (List Memory)
    , timelineMemories : Status (List Memory)
    , onThisDay : Status (List Memory)
    , stats : Status Stats
    , importantDays : Status (List ImportantDay)
    , relationship : Status RelationshipData
    , leftProfile : Status ProfileData
    , rightProfile : Status ProfileData
    , notes : Status (List CoupleNote)
    , plans : Status (List CouplePlan)
    , filter : Filter
    , tagInput : String
    , photoInput : String
    , addressSuggestions : List String
    , geocodeSeq : Int
    , relationshipDate : String
    , relationshipValidation : Bool
    , heartColor : String
    , leftAvatar : AvatarProfile
    , rightAvatar : AvatarProfile
    , editingAvatar : Maybe AvatarSide
    , editingHeart : Bool
    , memForm : MemForm
    , importantDayForm : ImportantDayForm
    , noteForm : NoteForm
    , noteTab : NoteTab
    , noteSearch : String
    , planForm : PlanForm
    , planTab : PlanTab
    , categoryForm : CategoryForm
    , planCategories : List String
    , calYear : Int
    , calMonth : Int
    , selectedDay : Maybe String
    , confirmDialog : Maybe { message : String, action : ConfirmAction }
    , infoDialog : Maybe String
    , tagForm : TagLocationForm
    , tagCatalog : List String
    , locationForm : TagLocationForm
    , locationCatalog : List String
    , trashPanelOpen : Bool
    , trash : Status (List TrashEntry)
    , themeColor : String
    , themePickerOpen : Bool
    , editingRelationship : Bool
    , draftAvatar : Maybe AvatarProfile
    , diaryEntries : Status (List DiaryEntry)
    , diaryTab : DiaryTab
    , diaryForm : DiaryForm
    }


-- MSG -----------------------------------------------

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GoTo Page
    | ToggleLanguage
    | GotMemories (Result Http.Error (List Memory))
    | GotTimelineMemories (Result Http.Error (List Memory))
    | GotOnThisDay (Result Http.Error (List Memory))
    | GotStats (Result Http.Error Stats)
    | GotImportantDays (Result Http.Error (List ImportantDay))
    | GotRelationship (Result Http.Error RelationshipData)
    | GotLeftProfile (Result Http.Error ProfileData)
    | GotRightProfile (Result Http.Error ProfileData)
    | RelationshipSaved (Result Http.Error RelationshipData)
    | ProfileSaved String (Result Http.Error ProfileData)
    | MemoryCreated (Result Http.Error Memory)
    | MemoryUpdated (Result Http.Error ())
    | MemoryDeleted Int (Result Http.Error ())
    | ImportantDayCreated (Result Http.Error ImportantDay)
    | ImportantDayUpdated (Result Http.Error ())
    | ImportantDayDeleted Int (Result Http.Error ())
    | GotNotes (Result Http.Error (List CoupleNote))
    | NoteCreated (Result Http.Error CoupleNote)
    | NoteUpdated (Result Http.Error ())
    | NoteDeleted Int (Result Http.Error ())
    | GotPlans (Result Http.Error (List CouplePlan))
    | GotTagCatalog (Result Http.Error (List String))
    | GotLocationCatalog (Result Http.Error (List String))
    | GotPlanCategories (Result Http.Error (List String))
    | PlanCreated (Result Http.Error CouplePlan)
    | PlanUpdated (Result Http.Error ())
    | PlanDeleted Int (Result Http.Error ())
    | OpenNewMemForm
    | OpenEditMemForm Memory
    | CloseMemForm
    | SetTitle String
    | SetTimeFrom String
    | SetTimeTo String
    | SetDescription String
    | SetLocation String
    | GotAddressSuggestions String (Result Http.Error (List String))
    | DoGeocodeIfCurrent Int String
    | SetTagInput String
    | AddTag
    | RemoveTag String
    | SetPhotoInput String
    | AddPhotoByName
    | SubmitMemForm
    | DeleteMemory Int
    | OpenNewImportantDayForm
    | OpenEditImportantDayForm ImportantDay
    | CloseImportantDayForm
    | SetImportantDayTitle String
    | SetImportantDayDate String
    | SetImportantDayNote String
    | SubmitImportantDayForm
    | DeleteImportantDay Int
    | OpenNewNoteForm
    | OpenEditNoteForm CoupleNote
    | CloseNoteForm
    | SetNoteOwner String
    | SetNoteTitle String
    | SetNoteBody String
    | SetNoteTab NoteTab
    | SetNoteSearch String
    | SubmitNoteForm
    | DeleteNote Int
    | OpenNewPlanForm
    | OpenEditPlanForm CouplePlan
    | ClosePlanForm
    | SetPlanCategory String
    | TogglePlanCategorySuggest
    | SelectPlanCategorySuggest String
    | SetPlanTitle String
    | SetPlanDetail String
    | SetPlanDone Bool
    | SubmitPlanForm
    | TogglePlanDone CouplePlan
    | DeletePlan Int
    | SetPlanTab PlanTab
    | OpenCategoryPanel
    | OpenCategoryPanelEdit String
    | CloseCategoryPanel
    | CancelCategoryEdit
    | SetCategoryNewName String
    | SetCategoryEditName String
    | SubmitNewCategory
    | SubmitCategoryRename
    | DeleteCategory String PlanTab
    | RequestConfirm ConfirmAction String
    | DoConfirm
    | CancelConfirm
    | CloseInfoDialog
    | SetRelationshipDate String
    | SetHeartColor String
    | OpenHeartEditor
    | CloseHeartEditor
    | SubmitRelationship
    | SetAvatarName AvatarSide String
    | SetAvatarMode AvatarSide String
    | SetAvatarPhoto AvatarSide String
    | SetAvatarFigureColor AvatarSide String
    | SetAvatarAccessory AvatarSide String
    | SetAvatarExpression AvatarSide String
    | SetAvatarBirthday AvatarSide String
    | PickAvatarPhoto AvatarSide
    | AvatarPhotoSelected AvatarSide File (List File)
    | AvatarPhotoUploaded AvatarSide (Result Http.Error (List String))
    | OpenAvatarEditor AvatarSide
    | CloseAvatarEditor
    | SubmitAvatar AvatarSide
    | SetFilterQ String
    | SetFilterTag String
    | SetFilterLocation String
    | SetFilterFrom String
    | SetFilterTo String
    | ClearFilters
    | PrevMonth
    | NextMonth
    | SelectDay String
    | PickPhotos
    | FilesSelected File (List File)
    | PhotosUploaded (Result Http.Error (List String))
    | RemovePhoto String
    | AddTagFromCombo String
    | ToggleLocSuggest
    | SelectLocSuggest String
    | ToggleTagSuggest
    | NoOp
    | OpenTagPanel
    | CloseTagPanel
    | StartEditTag String
    | CancelEditTag
    | SetTagEditName String
    | SubmitTagRename
    | DeleteTagFromAll String
    | SetTagNewName String
    | SubmitNewTag
    | OpenLocationPanel
    | CloseLocationPanel
    | StartEditLocation String
    | CancelEditLocation
    | SetLocationEditName String
    | SubmitLocationRename
    | DeleteLocationFromAll String
    | SetLocationNewName String
    | SubmitNewLocation
    | OpenTrashPanel
    | CloseTrashPanel
    | GotTrash (Result Http.Error (List TrashEntry))
    | RestoreTrashItem Int
    | TrashItemRestored Int (Result Http.Error ())
    | DeleteTrashItemPermanently Int
    | TrashItemDeleted Int (Result Http.Error ())
    | SetThemeColor String
    | OpenThemePicker
    | CloseThemePicker
    | OpenRelationshipEditor
    | CloseRelationshipEditor
    | TagRenamed (Result Http.Error ())
    | TagDeleted (Result Http.Error ())
    | TagCreated (Result Http.Error ())
    | LocationRenamed (Result Http.Error ())
    | LocationDeleted (Result Http.Error ())
    | LocationCreated (Result Http.Error ())
    | PlanCategoryRenamed (Result Http.Error ())
    | PlanCategoryDeleted (Result Http.Error ())
    | PlanCategoryCreated (Result Http.Error ())
    | GotDiary (Result Http.Error (List DiaryEntry))
    | DiaryEntryCreated (Result Http.Error DiaryEntry)
    | DiaryEntryUpdated (Result Http.Error ())
    | DiaryEntryDeleted Int (Result Http.Error ())
    | OpenNewDiaryEntry
    | OpenEditDiaryEntry DiaryEntry
    | CloseDiaryForm
    | SetDiaryDate String
    | SetDiaryBody String
    | SubmitDiaryForm
    | DeleteDiaryEntry Int
    | SetDiaryTab DiaryTab
