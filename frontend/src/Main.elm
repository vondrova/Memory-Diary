module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, top)


-- I18N (string-key approach, will be refactored to typed record later)

type Lang
    = CZ
    | EN


tr : Lang -> String -> String
tr lang key =
    case lang of
        CZ ->
            case key of
                "nav_memories"      -> "Vzpomínky"
                "nav_timeline"      -> "Timeline"
                "nav_important"     -> "Důležité dny"
                "nav_notes"         -> "Poznámky"
                "nav_plans"         -> "Plány"
                "nav_diary"         -> "Deník"
                "nav_stats"         -> "Statistiky"
                "nav_trash"         -> "Koš"
                "add_memory"        -> "+ Přidat vzpomínku"
                "save"              -> "Uložit"
                "cancel"            -> "Zrušit"
                "search"            -> "Hledat…"
                "empty"             -> "Nic tu není."
                "new_memory"        -> "Nová vzpomínka"
                "name"              -> "Název"
                "from"              -> "Od"
                "to"                -> "Do"
                "place"             -> "Místo"
                "tags"              -> "Tagy (oddělené čárkou)"
                "description"       -> "Popis"
                "photo"             -> "Fotografie"
                "pick_photo"        -> "📷 Vybrat foto"
                "map_search"        -> "Hledat na mapě…"
                "add_day"           -> "Přidat důležitý den"
                "date"              -> "Datum"
                "kind"              -> "Typ"
                "anniversary"       -> "💍 Výročí"
                "birthday"          -> "🎂 Narozeniny"
                "other_kind"        -> "📅 Jiné"
                "new_note"          -> "Nová poznámka"
                "from_who"          -> "Od koho"
                "title"             -> "Nadpis"
                "body"              -> "Text"
                "new_plan"          -> "Nový plán"
                "category"          -> "Kategorie"
                "detail"            -> "Detail"
                "done"              -> "Hotovo"
                "new_entry"         -> "Nový zápis"
                "mood"              -> "Nálada"
                "weather"           -> "Počasí"
                "statistics"        -> "Statistiky"
                "memory_count"      -> "Vzpomínek"
                "total_minutes"     -> "Celkem minut"
                "avg_minutes"       -> "Průměr minut"
                "visited_places"    -> "Navštívená místa"
                "top_tags"          -> "Nejčastější tagy"
                "trash_title"       -> "Koš"
                "trash_empty"       -> "Koš je prázdný."
                "restore"           -> "Obnovit"
                "delete"            -> "Smazat"
                _                   -> key

        EN ->
            case key of
                "nav_memories"      -> "Memories"
                "nav_timeline"      -> "Timeline"
                "nav_important"     -> "Important Days"
                "nav_notes"         -> "Notes"
                "nav_plans"         -> "Plans"
                "nav_diary"         -> "Diary"
                "nav_stats"         -> "Statistics"
                "nav_trash"         -> "Trash"
                "add_memory"        -> "+ Add memory"
                "save"              -> "Save"
                "cancel"            -> "Cancel"
                "search"            -> "Search…"
                "empty"             -> "Nothing here yet."
                "new_memory"        -> "New memory"
                "name"              -> "Name"
                "from"              -> "From"
                "to"                -> "To"
                "place"             -> "Place"
                "tags"              -> "Tags (comma-separated)"
                "description"       -> "Description"
                "photo"             -> "Photo"
                "pick_photo"        -> "📷 Pick photo"
                "map_search"        -> "Search on map…"
                "add_day"           -> "Add important day"
                "date"              -> "Date"
                "kind"              -> "Type"
                "anniversary"       -> "💍 Anniversary"
                "birthday"          -> "🎂 Birthday"
                "other_kind"        -> "📅 Other"
                "new_note"          -> "New note"
                "from_who"          -> "From"
                "title"             -> "Title"
                "body"              -> "Text"
                "new_plan"          -> "New plan"
                "category"          -> "Category"
                "detail"            -> "Detail"
                "done"              -> "Done"
                "new_entry"         -> "New entry"
                "mood"              -> "Mood"
                "weather"           -> "Weather"
                "statistics"        -> "Statistics"
                "memory_count"      -> "Memories"
                "total_minutes"     -> "Total minutes"
                "avg_minutes"       -> "Avg minutes"
                "visited_places"    -> "Visited places"
                "top_tags"          -> "Top tags"
                "trash_title"       -> "Trash"
                "trash_empty"       -> "Trash is empty."
                "restore"           -> "Restore"
                "delete"            -> "Delete"
                _                   -> key


-- THEME

type Theme
    = Light
    | Dark


themeClass : Theme -> String
themeClass t =
    case t of
        Light -> "theme-light"
        Dark  -> "theme-dark"


-- PAGES

type Page
    = HomePage
    | TimelinePage
    | ImportantDaysPage
    | NotesPage
    | PlansPage
    | DiaryPage
    | StatsPage
    | TrashPage
    | NotFoundPage


-- DOMAIN TYPES

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


-- MODEL

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
    , stats : Maybe Stats
    }


-- MSG

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
    | PhotoUploaded (Result Http.Error (List String))
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
    | GotStats (Result Http.Error Stats)


-- ROUTING

routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map HomePage top
        , Parser.map TimelinePage (Parser.s "timeline")
        , Parser.map ImportantDaysPage (Parser.s "important-days")
        , Parser.map NotesPage (Parser.s "notes")
        , Parser.map PlansPage (Parser.s "plans")
        , Parser.map DiaryPage (Parser.s "diary")
        , Parser.map StatsPage (Parser.s "stats")
        , Parser.map TrashPage (Parser.s "trash")
        ]


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault NotFoundPage (Parser.parse routeParser url)


-- INIT

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
      , stats = Nothing
      }
    , Cmd.batch [ fetchMemories, fetchImportantDays, fetchNotes, fetchPlans, fetchDiary, fetchStats ]
    )


-- UPDATE

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

        GotMemories (Ok mems) ->
            ( { model | memories = mems, error = Nothing }, Cmd.none )

        GotMemories (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst vzpomínky." }, Cmd.none )

        GotImportantDays (Ok days) ->
            ( { model | importantDays = days }, Cmd.none )

        GotImportantDays (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst důležité dny." }, Cmd.none )

        GotNotes (Ok ns) ->
            ( { model | notes = ns }, Cmd.none )

        GotNotes (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst poznámky." }, Cmd.none )

        GotPlans (Ok ps) ->
            ( { model | plans = ps }, Cmd.none )

        GotPlans (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst plány." }, Cmd.none )

        GotDiary (Ok entries) ->
            ( { model | diaryEntries = entries }, Cmd.none )

        GotDiary (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst deník." }, Cmd.none )

        SetSearch q ->
            ( { model | searchQuery = q }, Cmd.none )

        OpenForm ->
            ( { model | formOpen = True }, Cmd.none )

        CloseForm ->
            ( { model | formOpen = False, geoSuggestions = [], geoQuery = "" }, Cmd.none )

        SetTitle v -> ( { model | formTitle = v }, Cmd.none )
        SetTimeFrom v -> ( { model | formTimeFrom = v }, Cmd.none )
        SetTimeTo v -> ( { model | formTimeTo = v }, Cmd.none )
        SetDescription v -> ( { model | formDescription = v }, Cmd.none )
        SetLocation v -> ( { model | formLocation = v, geoSuggestions = [] }, Cmd.none )
        SetTags v -> ( { model | formTags = v }, Cmd.none )

        PickPhoto ->
            ( model, Select.file [ "image/*" ] PhotoPicked )

        PhotoPicked file ->
            ( { model | formPendingPhoto = Just file }, Cmd.none )

        PhotoUploaded _ ->
            ( { model | formPendingPhoto = Nothing }, Cmd.none )

        SetGeoQuery v -> ( { model | geoQuery = v }, Cmd.none )

        SearchGeo ->
            ( model, searchGeo model.geoQuery )

        GotGeoSuggestions (Ok sugg) ->
            ( { model | geoSuggestions = sugg }, Cmd.none )

        GotGeoSuggestions (Err _) ->
            ( { model | geoSuggestions = [] }, Cmd.none )

        SelectGeoSuggestion name ->
            ( { model | formLocation = name, geoSuggestions = [], geoQuery = "" }, Cmd.none )

        SubmitForm ->
            ( model, createMemory model )

        MemoryCreated (Ok mem) ->
            ( { model
                | memories = model.memories ++ [ mem ]
                , formOpen = False
                , formTitle = ""
                , formTimeFrom = ""
                , formTimeTo = ""
                , formDescription = ""
                , formLocation = ""
                , formTags = ""
                , formPendingPhoto = Nothing
              }
            , Cmd.none
            )

        MemoryCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit vzpomínku." }, Cmd.none )

        DeleteMemory memId ->
            let
                ( trashed, kept ) = List.partition (\m -> m.id == memId) model.memories
            in
            ( { model | memories = kept, trashedMemories = model.trashedMemories ++ trashed }
            , Cmd.none
            )

        RestoreMemory memId ->
            let
                ( restored, stillTrashed ) = List.partition (\m -> m.id == memId) model.trashedMemories
            in
            ( { model | trashedMemories = stillTrashed, memories = model.memories ++ restored }
            , Cmd.none
            )

        SetNewDayTitle v -> ( { model | newDayTitle = v }, Cmd.none )
        SetNewDayDate v -> ( { model | newDayDate = v }, Cmd.none )
        SetNewDayKind v -> ( { model | newDayKind = v }, Cmd.none )

        SubmitNewDay ->
            ( model, createImportantDay model )

        DayCreated (Ok day) ->
            ( { model | importantDays = model.importantDays ++ [ day ]
              , newDayTitle = "", newDayDate = "", newDayKind = "anniversary" }
            , Cmd.none
            )

        DayCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit důležitý den." }, Cmd.none )

        SetNoteOwner v -> ( { model | newNoteOwner = v }, Cmd.none )
        SetNoteTitle v -> ( { model | newNoteTitle = v }, Cmd.none )
        SetNoteBody v -> ( { model | newNoteBody = v }, Cmd.none )

        SubmitNote ->
            ( model, createNote model )

        NoteCreated (Ok note) ->
            ( { model | notes = model.notes ++ [ note ]
              , newNoteOwner = "", newNoteTitle = "", newNoteBody = "" }
            , Cmd.none
            )

        NoteCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit poznámku." }, Cmd.none )

        SetPlanCategory v -> ( { model | newPlanCategory = v }, Cmd.none )
        SetPlanTitle v -> ( { model | newPlanTitle = v }, Cmd.none )
        SetPlanDetail v -> ( { model | newPlanDetail = v }, Cmd.none )

        SubmitPlan ->
            ( model, createPlan model )

        PlanCreated (Ok plan) ->
            ( { model | plans = model.plans ++ [ plan ]
              , newPlanTitle = "", newPlanDetail = "" }
            , Cmd.none
            )

        PlanCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit plán." }, Cmd.none )

        TogglePlanDone plan ->
            ( model, updatePlan { plan | done = not plan.done } )

        PlanUpdated (Ok updated) ->
            ( { model | plans = List.map (\p -> if p.id == updated.id then updated else p) model.plans }
            , Cmd.none
            )

        PlanUpdated (Err _) ->
            ( { model | error = Just "Nepodařilo se aktualizovat plán." }, Cmd.none )

        SetDiaryDate v -> ( { model | newDiaryDate = v }, Cmd.none )
        SetDiaryBody v -> ( { model | newDiaryBody = v }, Cmd.none )
        SetDiaryMood v -> ( { model | newDiaryMood = v }, Cmd.none )

        SubmitDiaryEntry ->
            ( model, createDiaryEntry model )

        DiaryCreated (Ok entry) ->
            ( { model | diaryEntries = entry :: model.diaryEntries
              , newDiaryDate = "", newDiaryBody = "", newDiaryMood = "😊" }
            , Cmd.none
            )

        DiaryCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit zápis." }, Cmd.none )

        GotStats (Ok s) ->
            ( { model | stats = Just s }, Cmd.none )

        GotStats (Err _) ->
            ( model, Cmd.none )


-- HTTP

fetchMemories : Cmd Msg
fetchMemories =
    Http.get { url = "/api/memories", expect = Http.expectJson GotMemories (D.list memoryDecoder) }

fetchImportantDays : Cmd Msg
fetchImportantDays =
    Http.get { url = "/api/important-days", expect = Http.expectJson GotImportantDays (D.list importantDayDecoder) }

fetchNotes : Cmd Msg
fetchNotes =
    Http.get { url = "/api/notes", expect = Http.expectJson GotNotes (D.list coupleNoteDecoder) }

fetchPlans : Cmd Msg
fetchPlans =
    Http.get { url = "/api/plans", expect = Http.expectJson GotPlans (D.list couplePlanDecoder) }

fetchDiary : Cmd Msg
fetchDiary =
    Http.get { url = "/api/diary", expect = Http.expectJson GotDiary (D.list diaryDecoder) }

fetchStats : Cmd Msg
fetchStats =
    Http.get { url = "/api/stats", expect = Http.expectJson GotStats statsDecoder }

searchGeo : String -> Cmd Msg
searchGeo q =
    Http.get { url = "/api/geocode?q=" ++ q, expect = Http.expectJson GotGeoSuggestions (D.list geoDecoder) }

createMemory : Model -> Cmd Msg
createMemory model =
    Http.post { url = "/api/memories", body = Http.jsonBody (encodeMemoryInput model), expect = Http.expectJson MemoryCreated memoryDecoder }

createImportantDay : Model -> Cmd Msg
createImportantDay model =
    Http.post { url = "/api/important-days"
              , body = Http.jsonBody (E.object [ ( "title", E.string model.newDayTitle ), ( "date", E.string model.newDayDate ), ( "note", E.null ), ( "kind", E.string model.newDayKind ) ])
              , expect = Http.expectJson DayCreated importantDayDecoder }

createNote : Model -> Cmd Msg
createNote model =
    Http.post { url = "/api/notes"
              , body = Http.jsonBody (E.object [ ( "owner", E.string model.newNoteOwner ), ( "title", E.string model.newNoteTitle ), ( "body", E.string model.newNoteBody ) ])
              , expect = Http.expectJson NoteCreated coupleNoteDecoder }

createPlan : Model -> Cmd Msg
createPlan model =
    Http.post { url = "/api/plans"
              , body = Http.jsonBody (E.object [ ( "category", E.string model.newPlanCategory ), ( "title", E.string model.newPlanTitle ), ( "detail", if String.isEmpty model.newPlanDetail then E.null else E.string model.newPlanDetail ), ( "done", E.bool False ) ])
              , expect = Http.expectJson PlanCreated couplePlanDecoder }

updatePlan : CouplePlan -> Cmd Msg
updatePlan plan =
    case plan.id of
        Nothing -> Cmd.none
        Just planId ->
            Http.request { method = "PUT", headers = [], url = "/api/plans/" ++ String.fromInt planId
                         , body = Http.jsonBody (E.object [ ( "category", E.string plan.category ), ( "title", E.string plan.title ), ( "detail", Maybe.withDefault E.null (Maybe.map E.string plan.detail) ), ( "done", E.bool plan.done ) ])
                         , expect = Http.expectJson PlanUpdated couplePlanDecoder
                         , timeout = Nothing, tracker = Nothing }

createDiaryEntry : Model -> Cmd Msg
createDiaryEntry model =
    Http.post { url = "/api/diary"
              , body = Http.jsonBody (E.object [ ( "date", E.string model.newDiaryDate ), ( "mood", E.string model.newDiaryMood ), ( "body", E.string model.newDiaryBody ), ( "weather", E.null ) ])
              , expect = Http.expectJson DiaryCreated diaryDecoder }

encodeMemoryInput : Model -> E.Value
encodeMemoryInput model =
    let
        tags = model.formTags |> String.split "," |> List.map String.trim |> List.filter (not << String.isEmpty)
        nullable s = if String.isEmpty s then E.null else E.string s
    in
    E.object [ ( "inputTitle", E.string model.formTitle ), ( "inputTimeFrom", E.string model.formTimeFrom ), ( "inputTimeTo", E.string model.formTimeTo ), ( "inputDescription", nullable model.formDescription ), ( "inputLocation", nullable model.formLocation ), ( "inputTags", E.list E.string tags ), ( "inputPhotos", E.list E.string [] ) ]


-- DECODERS

memoryDecoder : D.Decoder Memory
memoryDecoder =
    D.map8 Memory
        (D.field "memoryId" D.int)
        (D.field "memoryTitle" D.string)
        (D.field "memoryTimeFrom" D.string)
        (D.field "memoryTimeTo" D.string)
        (D.maybe (D.field "memoryDescription" D.string))
        (D.maybe (D.field "memoryLocation" D.string))
        (D.field "memoryTags" (D.list D.string))
        (D.field "memoryPhotos" (D.list D.string))

importantDayDecoder : D.Decoder ImportantDay
importantDayDecoder =
    D.map5 ImportantDay (D.maybe (D.field "id" D.int)) (D.field "title" D.string) (D.field "date" D.string) (D.maybe (D.field "note" D.string)) (D.field "kind" D.string)

coupleNoteDecoder : D.Decoder CoupleNote
coupleNoteDecoder =
    D.map5 CoupleNote (D.maybe (D.field "id" D.int)) (D.field "owner" D.string) (D.field "title" D.string) (D.field "body" D.string) (D.field "createdAt" D.string)

couplePlanDecoder : D.Decoder CouplePlan
couplePlanDecoder =
    D.map5 CouplePlan (D.maybe (D.field "id" D.int)) (D.field "category" D.string) (D.field "title" D.string) (D.maybe (D.field "detail" D.string)) (D.field "done" D.bool)

diaryDecoder : D.Decoder DiaryEntry
diaryDecoder =
    D.map5 DiaryEntry (D.maybe (D.field "id" D.int)) (D.field "date" D.string) (D.maybe (D.field "mood" D.string)) (D.field "body" D.string) (D.maybe (D.field "weather" D.string))

geoDecoder : D.Decoder GeoSuggestion
geoDecoder =
    D.map3 GeoSuggestion (D.field "displayName" D.string) (D.field "lat" D.string) (D.field "lon" D.string)

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


-- CLIENT-SIDE BUSINESS LOGIC

filterMemories : String -> List Memory -> List Memory
filterMemories query mems =
    if String.isEmpty query then mems
    else
        let q = String.toLower query
        in List.filter (\m -> String.contains q (String.toLower m.title) || List.any (String.contains q << String.toLower) m.tags || Maybe.withDefault False (Maybe.map (String.contains q << String.toLower) m.location)) mems

countTags : List Memory -> List ( String, Int )
countTags mems =
    List.foldl (\tag acc -> case List.partition (\( k, _ ) -> k == tag) acc of
        ( [], rest ) -> ( tag, 1 ) :: rest
        ( ( k, n ) :: _, rest ) -> ( k, n + 1 ) :: rest) [] (List.concatMap .tags mems)
        |> List.sortBy (negate << Tuple.second)

parseDatetimeMinutes : String -> Maybe Int
parseDatetimeMinutes s =
    case String.split "T" s of
        [ dp, tp ] -> case ( String.split "-" dp, String.split ":" tp ) of
            ( [ y, mo, d ], [ h, mi ] ) -> Maybe.map5 (\yr month day hour minute -> yr * 365 * 24 * 60 + month * 30 * 24 * 60 + day * 24 * 60 + hour * 60 + minute) (String.toInt y) (String.toInt mo) (String.toInt d) (String.toInt h) (String.toInt mi)
            _ -> Nothing
        _ -> Nothing

durationMinutes : Memory -> Int
durationMinutes mem =
    case ( parseDatetimeMinutes mem.timeFrom, parseDatetimeMinutes mem.timeTo ) of
        ( Just f, Just t ) -> Basics.max 0 (t - f)
        _ -> 0

computeStats : List Memory -> { total : Int, count : Int, places : Int, avgMins : Int }
computeStats mems =
    let
        total = List.sum (List.map durationMinutes mems)
        places = mems |> List.filterMap .location |> List.sort |> List.foldl (\x acc -> if List.member x acc then acc else acc ++ [ x ]) [] |> List.length
        avg = if List.isEmpty mems then 0 else total // List.length mems
    in { total = total, count = List.length mems, places = places, avgMins = avg }


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Memory Diary"
    , body =
        [ div [ class ("app " ++ themeClass model.theme) ]
            [ viewHeader model
            , main_ [ class "main" ]
                [ case model.error of
                    Just err -> div [ class "error" ] [ text err ]
                    Nothing  -> text ""
                , viewPage model
                ]
            ]
        ]
    }

viewHeader : Model -> Html Msg
viewHeader model =
    let t = tr model.lang
    in
    header [ class "header" ]
        [ h1 [ class "header-title" ] [ text "Memory Diary" ]
        , nav [ class "nav" ]
            [ navLink "/" (t "nav_memories") (model.page == HomePage)
            , navLink "/timeline" (t "nav_timeline") (model.page == TimelinePage)
            , navLink "/important-days" (t "nav_important") (model.page == ImportantDaysPage)
            , navLink "/notes" (t "nav_notes") (model.page == NotesPage)
            , navLink "/plans" (t "nav_plans") (model.page == PlansPage)
            , navLink "/diary" (t "nav_diary") (model.page == DiaryPage)
            , navLink "/stats" (t "nav_stats") (model.page == StatsPage)
            , navLink "/trash" (t "nav_trash") (model.page == TrashPage)
            ]
        , div [ class "header-controls" ]
            [ button [ class "btn-icon", onClick ToggleLang ]
                [ text (if model.lang == CZ then "EN" else "CZ") ]
            , button [ class "btn-icon", onClick ToggleTheme ]
                [ text (if model.theme == Light then "🌙" else "☀️") ]
            ]
        ]

navLink : String -> String -> Bool -> Html Msg
navLink href_ label_ active =
    a [ href href_, class (if active then "nav-link nav-link--active" else "nav-link") ] [ text label_ ]

viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        HomePage          -> viewHome model
        TimelinePage      -> div [ class "timeline" ] (List.map viewTimelineItem model.memories)
        ImportantDaysPage -> viewImportantDays model
        NotesPage         -> viewNotes model
        PlansPage         -> viewPlans model
        DiaryPage         -> viewDiary model
        StatsPage         -> viewStats model
        TrashPage         -> viewTrash model
        NotFoundPage      -> div [ class "not-found" ] [ text "Stránka nenalezena." ]

viewHome : Model -> Html Msg
viewHome model =
    let
        t        = tr model.lang
        filtered = filterMemories model.searchQuery model.memories
    in
    div []
        [ div [ class "page-actions" ]
            [ input [ type_ "search", placeholder (t "search"), value model.searchQuery, onInput SetSearch, class "search-input" ] []
            , button [ class "btn-primary", onClick OpenForm ] [ text (t "add_memory") ]
            ]
        , if model.formOpen then viewForm model else text ""
        , if List.isEmpty filtered then p [ class "empty-state" ] [ text (t "empty") ]
          else div [ class "memory-list" ] (List.map (viewMemory model.lang) filtered)
        ]

viewForm : Model -> Html Msg
viewForm model =
    let t = tr model.lang
    in
    div [ class "form-overlay" ]
        [ Html.form [ class "memory-form", onSubmit SubmitForm ]
            [ h2 [] [ text (t "new_memory") ]
            , label [] [ text (t "name") ]
            , input [ type_ "text", value model.formTitle, onInput SetTitle, required True ] []
            , label [] [ text (t "from") ]
            , input [ type_ "datetime-local", value model.formTimeFrom, onInput SetTimeFrom, required True ] []
            , label [] [ text (t "to") ]
            , input [ type_ "datetime-local", value model.formTimeTo, onInput SetTimeTo, required True ] []
            , label [] [ text (t "place") ]
            , div [ class "geo-row" ]
                [ input [ type_ "text", value model.formLocation, onInput SetLocation, placeholder (t "place") ] []
                , input [ type_ "text", value model.geoQuery, onInput SetGeoQuery, placeholder (t "map_search"), class "geo-input" ] []
                , button [ type_ "button", class "btn-secondary", onClick SearchGeo ] [ text "🔍" ]
                ]
            , if List.isEmpty model.geoSuggestions then text ""
              else div [ class "geo-suggestions" ] (List.map (\s -> div [ class "geo-suggestion", onClick (SelectGeoSuggestion s.displayName) ] [ text s.displayName ]) model.geoSuggestions)
            , label [] [ text (t "tags") ]
            , input [ type_ "text", value model.formTags, onInput SetTags ] []
            , label [] [ text (t "description") ]
            , textarea [ value model.formDescription, onInput SetDescription ] []
            , label [] [ text (t "photo") ]
            , div [ class "photo-row" ]
                [ button [ type_ "button", class "btn-secondary", onClick PickPhoto ] [ text (t "pick_photo") ]
                , case model.formPendingPhoto of
                    Just f  -> span [ class "photo-name" ] [ text (File.name f) ]
                    Nothing -> text ""
                ]
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ]
                , button [ type_ "button", class "btn-secondary", onClick CloseForm ] [ text (t "cancel") ]
                ]
            ]
        ]

viewMemory : Lang -> Memory -> Html Msg
viewMemory lang mem =
    let t = tr lang
    in
    div [ class "memory-card" ]
        [ div [ class "memory-card-header" ]
            [ h2 [ class "memory-title" ] [ text mem.title ]
            , button [ class "btn-icon btn-delete", onClick (DeleteMemory mem.id) ] [ text "🗑" ]
            ]
        , p [ class "memory-time" ] [ text (mem.timeFrom ++ " – " ++ mem.timeTo) ]
        , case mem.location of
            Just loc -> p [ class "memory-location" ] [ text ("📍 " ++ loc) ]
            Nothing  -> text ""
        , case mem.description of
            Just desc -> p [ class "memory-desc" ] [ text desc ]
            Nothing   -> text ""
        , div [ class "memory-tags" ] (List.map (\tag -> span [ class "tag" ] [ text tag ]) mem.tags)
        ]

viewTimelineItem : Memory -> Html Msg
viewTimelineItem mem =
    div [ class "timeline-item" ]
        [ div [ class "timeline-dot" ] []
        , div [ class "timeline-content" ]
            [ span [ class "timeline-date" ] [ text mem.timeFrom ]
            , strong [] [ text mem.title ]
            , case mem.location of
                Just loc -> span [ class "timeline-location" ] [ text (" · " ++ loc) ]
                Nothing  -> text ""
            ]
        ]

viewImportantDays : Model -> Html Msg
viewImportantDays model =
    let t = tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitNewDay, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "add_day") ]
            , label [] [ text (t "name") ], input [ type_ "text", value model.newDayTitle, onInput SetNewDayTitle, required True ] []
            , label [] [ text (t "date") ], input [ type_ "date", value model.newDayDate, onInput SetNewDayDate, required True ] []
            , label [] [ text (t "kind") ]
            , select [ onInput SetNewDayKind ]
                [ option [ value "anniversary" ] [ text (t "anniversary") ]
                , option [ value "birthday" ] [ text (t "birthday") ]
                , option [ value "other" ] [ text (t "other_kind") ]
                ]
            , div [ class "form-actions" ] [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map viewImportantDay (List.sortBy .date model.importantDays))
        ]

viewImportantDay : ImportantDay -> Html Msg
viewImportantDay day =
    div [ class "memory-card" ]
        [ h2 [ class "memory-title" ] [ text day.title ]
        , p [ class "memory-time" ] [ text (day.kind ++ "  ·  " ++ day.date) ]
        , case day.note of
            Just n  -> p [ class "memory-desc" ] [ text n ]
            Nothing -> text ""
        ]

viewNotes : Model -> Html Msg
viewNotes model =
    let t = tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitNote, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "new_note") ]
            , label [] [ text (t "from_who") ], input [ type_ "text", value model.newNoteOwner, onInput SetNoteOwner, required True ] []
            , label [] [ text (t "title") ], input [ type_ "text", value model.newNoteTitle, onInput SetNoteTitle, required True ] []
            , label [] [ text (t "body") ], textarea [ value model.newNoteBody, onInput SetNoteBody, required True ] []
            , div [ class "form-actions" ] [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map viewNote model.notes)
        ]

viewNote : CoupleNote -> Html Msg
viewNote note =
    div [ class "memory-card" ]
        [ h2 [ class "memory-title" ] [ text note.title ]
        , p [ class "memory-time" ] [ text (note.owner ++ "  ·  " ++ note.createdAt) ]
        , p [ class "memory-desc" ] [ text note.body ]
        ]

viewPlans : Model -> Html Msg
viewPlans model =
    let t = tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitPlan, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "new_plan") ]
            , label [] [ text (t "category") ]
            , select [ onInput SetPlanCategory ]
                [ option [ value "travel" ] [ text "✈️ Cestování" ]
                , option [ value "food" ] [ text "🍕 Jídlo" ]
                , option [ value "activity" ] [ text "🎯 Aktivita" ]
                , option [ value "other" ] [ text "📌 Jiné" ]
                ]
            , label [] [ text (t "name") ], input [ type_ "text", value model.newPlanTitle, onInput SetPlanTitle, required True ] []
            , label [] [ text (t "detail") ], input [ type_ "text", value model.newPlanDetail, onInput SetPlanDetail ] []
            , div [ class "form-actions" ] [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map (viewPlan model.lang) model.plans)
        ]

viewPlan : Lang -> CouplePlan -> Html Msg
viewPlan lang plan =
    let t = tr lang
    in
    div [ class ("memory-card" ++ if plan.done then " plan-done" else "") ]
        [ div [ class "memory-card-header" ]
            [ h2 [ class "memory-title" ] [ text plan.title ]
            , button [ class "btn-icon", onClick (TogglePlanDone plan) ]
                [ text (if plan.done then "✅" else "⬜") ]
            ]
        , p [ class "memory-time" ] [ text plan.category ]
        , case plan.detail of
            Just d  -> p [ class "memory-desc" ] [ text d ]
            Nothing -> text ""
        ]

viewDiary : Model -> Html Msg
viewDiary model =
    let t = tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitDiaryEntry, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "new_entry") ]
            , label [] [ text (t "date") ], input [ type_ "date", value model.newDiaryDate, onInput SetDiaryDate, required True ] []
            , label [] [ text (t "mood") ]
            , select [ onInput SetDiaryMood ]
                [ option [ value "😊" ] [ text "😊 Šťastný/á" ]
                , option [ value "😐" ] [ text "😐 Neutrální" ]
                , option [ value "😢" ] [ text "😢 Smutný/á" ]
                , option [ value "😴" ] [ text "😴 Unavený/á" ]
                , option [ value "🥰" ] [ text "🥰 Zamilovaný/á" ]
                ]
            , label [] [ text (t "body") ], textarea [ value model.newDiaryBody, onInput SetDiaryBody, required True, rows 5 ] []
            , div [ class "form-actions" ] [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map viewDiaryEntry model.diaryEntries)
        ]

viewDiaryEntry : DiaryEntry -> Html Msg
viewDiaryEntry entry =
    div [ class "memory-card" ]
        [ div [ class "memory-card-header" ]
            [ h2 [ class "memory-title" ] [ text entry.date ]
            , case entry.mood of
                Just m  -> span [ class "mood-badge" ] [ text m ]
                Nothing -> text ""
            ]
        , p [ class "memory-desc" ] [ text entry.body ]
        ]

viewTrash : Model -> Html Msg
viewTrash model =
    let t = tr model.lang
    in
    div []
        [ h2 [ style "margin-bottom" "1rem" ] [ text (t "trash_title") ]
        , if List.isEmpty model.trashedMemories then
            p [ class "empty-state" ] [ text (t "trash_empty") ]
          else
            div [ class "memory-list" ]
                (List.map
                    (\mem -> div [ class "memory-card memory-card--trashed" ]
                        [ div [ class "memory-card-header" ]
                            [ h2 [ class "memory-title" ] [ text mem.title ]
                            , button [ class "btn-secondary", onClick (RestoreMemory mem.id) ] [ text (t "restore") ]
                            ]
                        , p [ class "memory-time" ] [ text mem.timeFrom ]
                        ])
                    model.trashedMemories)
        ]

viewStats : Model -> Html Msg
viewStats model =
    let t = tr model.lang
    in
    case model.stats of
        Nothing ->
            p [ class "empty-state" ] [ text "Načítám statistiky…" ]

        Just s ->
            div [ class "stats-page" ]
                [ h2 [] [ text (t "statistics") ]
                , div [ class "stats-grid" ]
                    [ statCard (t "memory_count") (String.fromInt s.memoryCount)
                    , statCard (t "total_minutes") (String.fromInt s.totalMinutes)
                    , statCard (t "avg_minutes") (String.fromInt s.averageMinutes)
                    , statCard (t "visited_places") (String.fromInt s.visitedPlaces)
                    ]
                , h3 [] [ text (t "top_tags") ]
                , ul [ class "tag-list" ]
                    (List.map
                        (\( tag, count ) ->
                            li [] [ span [ class "tag" ] [ text tag ], text (" × " ++ String.fromInt count) ])
                        (List.take 5 s.topTags))
                ]

statCard : String -> String -> Html Msg
statCard label_ value_ =
    div [ class "stat-card" ]
        [ div [ class "stat-value" ] [ text value_ ]
        , div [ class "stat-label" ] [ text label_ ]
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
