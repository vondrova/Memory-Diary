module Routing exposing
    ( languageFromUrl
    , pageFromUrl
    , pageUrl
    , pageUrlFor
    )

{-| Hash-based URL routing for the Memory Diary
-}

import Types exposing (Language(..), Page(..))
import Url exposing (Url)


-- URL GENERATION -----------------------------------------------

-- | Default English URL for a page; used where language context is unavailable
pageUrl : Page -> String
pageUrl page =
    pageUrlFor English page


-- | Hash-fragment URL for a page in the given language
pageUrlFor : Language -> Page -> String
pageUrlFor lang page =
    case page of
        HomePage ->
            "#/"

        TimelinePage ->
            "#/timeline"

        CalendarPage ->
            case lang of
                English ->
                    "#/calendar"

                Czech ->
                    "#/kalendar"

        StatsPage ->
            case lang of
                English ->
                    "#/stats"

                Czech ->
                    "#/statistiky"

        ImportantDaysPage ->
            case lang of
                English ->
                    "#/important-days"

                Czech ->
                    "#/dulezite-dny"

        NotesPage ->
            case lang of
                English ->
                    "#/notes"

                Czech ->
                    "#/poznamky"

        PlansPage ->
            case lang of
                English ->
                    "#/plans"

                Czech ->
                    "#/plany"

        DiaryPage ->
            case lang of
                English ->
                    "#/diary"

                Czech ->
                    "#/denik"


-- URL PARSING -----------------------------------------------

-- | Map a URL fragment to a Page, unknown fragments fall back to HomePage
pageFromUrl : Url -> Page
pageFromUrl url =
    case Maybe.withDefault "/" url.fragment of
        "/" ->
            HomePage

        "" ->
            HomePage

        "/timeline" ->
            TimelinePage

        "timeline" ->
            TimelinePage

        "/kalendar" ->
            CalendarPage

        "kalendar" ->
            CalendarPage

        "/calendar" ->
            CalendarPage

        "calendar" ->
            CalendarPage

        "/statistiky" ->
            StatsPage

        "statistiky" ->
            StatsPage

        "/stats" ->
            StatsPage

        "stats" ->
            StatsPage

        "/milniky" ->
            ImportantDaysPage

        "milniky" ->
            ImportantDaysPage

        "/dulezite-dny" ->
            ImportantDaysPage

        "dulezite-dny" ->
            ImportantDaysPage

        "/important-days" ->
            ImportantDaysPage

        "important-days" ->
            ImportantDaysPage

        "/poznamky" ->
            NotesPage

        "poznamky" ->
            NotesPage

        "/notes" ->
            NotesPage

        "notes" ->
            NotesPage

        "/plany" ->
            PlansPage

        "plany" ->
            PlansPage

        "/plans" ->
            PlansPage

        "plans" ->
            PlansPage

        "/denik" ->
            DiaryPage

        "denik" ->
            DiaryPage

        "/diary" ->
            DiaryPage

        "diary" ->
            DiaryPage

        _ ->
            HomePage


-- | Detect language from a URL fragment, returns Nothing for language-neutral pages (home, timeline)
languageFromUrl : Url -> Maybe Language
languageFromUrl url =
    case Maybe.withDefault "/" url.fragment of
        "/kalendar" ->
            Just Czech

        "kalendar" ->
            Just Czech

        "/calendar" ->
            Just English

        "calendar" ->
            Just English

        "/statistiky" ->
            Just Czech

        "statistiky" ->
            Just Czech

        "/stats" ->
            Just English

        "stats" ->
            Just English

        "/milniky" ->
            Just Czech

        "milniky" ->
            Just Czech

        "/dulezite-dny" ->
            Just Czech

        "dulezite-dny" ->
            Just Czech

        "/important-days" ->
            Just English

        "important-days" ->
            Just English

        "/poznamky" ->
            Just Czech

        "poznamky" ->
            Just Czech

        "/notes" ->
            Just English

        "notes" ->
            Just English

        "/plany" ->
            Just Czech

        "plany" ->
            Just Czech

        "/plans" ->
            Just English

        "plans" ->
            Just English

        "/denik" ->
            Just Czech

        "denik" ->
            Just Czech

        "/diary" ->
            Just English

        "diary" ->
            Just English

        _ ->
            Nothing
