{- All shared types for the Memory Diary frontend.
   Domain types, page routing, Model, and Msg all live here
   so that no project module needs to import another project module
   (which would create circular dependencies).
-}
module Types exposing
    ( Memory
    , ImportantDay
    , CoupleNote
    , CouplePlan
    , DiaryEntry
    , GeoSuggestion
    , Stats
    , Lang(..)
    , Theme(..)
    , themeClass
    , Page(..)
    , Model
    , Msg(..)
    )

import Browser
import Browser.Navigation as Nav
import File exposing (File)
import Http
import Url exposing (Url)


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
    }


type alias CoupleNote =
    { id : Maybe Int
    , owner : String
    , title : String
    , body : String
    , createdAt : String
    }


type alias CouplePlan =
    { id : Maybe Int
    , category : String
    , title : String
    , detail : Maybe String
    , done : Bool
    }


type alias DiaryEntry =
    { id : Maybe Int
    , date : String
    , mood : Maybe String
    , body : String
    , weather : Maybe String
    }


type alias GeoSuggestion =
    { displayName : String
    , lat : String
    , lon : String
    }


type alias Stats =
    { totalMinutes : Int
    , memoryCount : Int
    , photoCount : Int
    , visitedPlaces : Int
    , averageMinutes : Int
    , topTags : List ( String, Int )
    , topLocations : List ( String, Int )
    }


-- UI TYPES -----------------------------------------------

type Lang
    = CZ
    | EN


type Theme
    = Light
    | Dark


themeClass : Theme -> String
themeClass t =
    case t of
        Light -> "theme-light"
        Dark  -> "theme-dark"


-- ROUTING -----------------------------------------------

type Page
    = HomePage
    | TimelinePage
    | ImportantDaysPage
    | NotesPage
    | PlansPage
    | DiaryPage
    | StatsPage
    | TrashPage
    | CalendarPage
    | NotFoundPage


-- MODEL -----------------------------------------------

type alias Model =
    { key : Nav.Key
    , page : Page
    , lang : Lang
    , theme : Theme
    , memories : List Memory
    , trashedMemories : List Memory
    , importantDays : List ImportantDay
    , notes : List CoupleNote
    , plans : List CouplePlan
    , diaryEntries : List DiaryEntry
    , stats : Maybe Stats
    , searchQuery : String
    , error : Maybe String
    , formOpen : Bool
    , formTitle : String
    , formTimeFrom : String
    , formTimeTo : String
    , formDescription : String
    , formLocation : String
    , formTags : String
    , formPendingPhoto : Maybe File
    , geoQuery : String
    , geoSuggestions : List GeoSuggestion
    , newDayTitle : String
    , newDayDate : String
    , newDayKind : String
    , newNoteOwner : String
    , newNoteTitle : String
    , newNoteBody : String
    , newPlanCategory : String
    , newPlanTitle : String
    , newPlanDetail : String
    , newDiaryDate : String
    , newDiaryBody : String
    , newDiaryMood : String
    }


-- MSG -----------------------------------------------

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ToggleLang
    | ToggleTheme
    | GotMemories (Result Http.Error (List Memory))
    | GotImportantDays (Result Http.Error (List ImportantDay))
    | GotNotes (Result Http.Error (List CoupleNote))
    | GotPlans (Result Http.Error (List CouplePlan))
    | GotDiary (Result Http.Error (List DiaryEntry))
    | GotStats (Result Http.Error Stats)
    | SetSearch String
    | OpenForm
    | CloseForm
    | SetTitle String
    | SetTimeFrom String
    | SetTimeTo String
    | SetDescription String
    | SetLocation String
    | SetTags String
    | PickPhoto
    | PhotoPicked File
    | SetGeoQuery String
    | SearchGeo
    | GotGeoSuggestions (Result Http.Error (List GeoSuggestion))
    | SelectGeoSuggestion String
    | SubmitForm
    | MemoryCreated (Result Http.Error Memory)
    | DeleteMemory Int
    | RestoreMemory Int
    | SetNewDayTitle String
    | SetNewDayDate String
    | SetNewDayKind String
    | SubmitNewDay
    | DayCreated (Result Http.Error ImportantDay)
    | SetNoteOwner String
    | SetNoteTitle String
    | SetNoteBody String
    | SubmitNote
    | NoteCreated (Result Http.Error CoupleNote)
    | SetPlanCategory String
    | SetPlanTitle String
    | SetPlanDetail String
    | SubmitPlan
    | PlanCreated (Result Http.Error CouplePlan)
    | TogglePlanDone CouplePlan
    | PlanUpdated (Result Http.Error CouplePlan)
    | SetDiaryDate String
    | SetDiaryBody String
    | SetDiaryMood String
    | SubmitDiaryEntry
    | DiaryCreated (Result Http.Error DiaryEntry)
