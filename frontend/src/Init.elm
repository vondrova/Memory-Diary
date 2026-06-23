{- Initial application state. -}
module Init exposing (init)

import Api.Requests as Requests
import Browser.Navigation as Nav
import Routing exposing (fromUrl)
import Types exposing (..)
import Url exposing (Url)


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , page = fromUrl url
      , lang = CZ
      , theme = Light
      , memories = []
      , trashedMemories = []
      , importantDays = []
      , notes = []
      , plans = []
      , diaryEntries = []
      , stats = Nothing
      , searchQuery = ""
      , error = Nothing
      , formOpen = False
      , formTitle = ""
      , formTimeFrom = ""
      , formTimeTo = ""
      , formDescription = ""
      , formLocation = ""
      , formTags = ""
      , formPendingPhoto = Nothing
      , geoQuery = ""
      , geoSuggestions = []
      , newDayTitle = ""
      , newDayDate = ""
      , newDayKind = "anniversary"
      , newNoteOwner = ""
      , newNoteTitle = ""
      , newNoteBody = ""
      , newPlanCategory = "travel"
      , newPlanTitle = ""
      , newPlanDetail = ""
      , newDiaryDate = ""
      , newDiaryBody = ""
      , newDiaryMood = "😊"
      }
    , Cmd.batch
        [ Requests.fetchMemories "" GotMemories
        , Requests.fetchImportantDays GotImportantDays
        , Requests.fetchNotes GotNotes
        , Requests.fetchPlans GotPlans
        , Requests.fetchDiary GotDiary
        , Requests.fetchStats GotStats
        ]
    )
