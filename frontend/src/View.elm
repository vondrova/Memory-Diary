{- Root view: header + page routing. -}
module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Language
import Page.Calendar exposing (viewCalendar)
import Page.Diary exposing (viewDiary)
import Page.Home exposing (viewHome)
import Page.ImportantDays exposing (viewImportantDays)
import Page.Notes exposing (viewNotes)
import Page.Plans exposing (viewPlans)
import Page.Stats exposing (viewStats)
import Page.Timeline exposing (viewTimeline)
import Page.Trash exposing (viewTrash)
import Types exposing (..)


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
    let
        t = Language.tr model.lang
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
        TimelinePage      -> viewTimeline model
        ImportantDaysPage -> viewImportantDays model
        NotesPage         -> viewNotes model
        PlansPage         -> viewPlans model
        DiaryPage         -> viewDiary model
        StatsPage         -> viewStats model
        TrashPage         -> viewTrash model
        CalendarPage      -> viewCalendar model
        NotFoundPage      -> div [ class "not-found" ] [ text "Stránka nenalezena." ]
