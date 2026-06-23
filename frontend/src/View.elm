module View exposing (view)

{-| Root view function for page routing and shared overlays
-}

import Browser
import Html exposing (Html, a, button, div, h3, input, nav, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Language exposing (confirmRemovePlace, confirmRemoveTag, languageCode, tr)
import Page.Calendar exposing (viewCalendar)
import Page.Diary exposing (viewDiary)
import Page.Home exposing (viewHome)
import Page.ImportantDays exposing (viewImportantDays)
import Page.Notes exposing (viewNotes)
import Page.Plans exposing (viewPlans)
import Page.Stats exposing (viewStats)
import Page.Timeline exposing (onEnter, viewTimeline)
import Routing exposing (pageUrlFor)
import Types exposing (..)



-- VIEW ROOT -----------------------------------------------
-- | Top-level document: wraps the active page view and all shared overlays


view : Model -> Browser.Document Msg
view model =
    { title = "Memory Diary"
    , body =
        [ div [ class ("app " ++ model.themeColor) ]
            [ viewNav model
            , case model.page of
                HomePage ->
                    viewHome model

                TimelinePage ->
                    viewTimeline model

                CalendarPage ->
                    viewCalendar model

                StatsPage ->
                    viewStats model

                ImportantDaysPage ->
                    viewImportantDays model

                NotesPage ->
                    viewNotes model

                PlansPage ->
                    viewPlans model

                DiaryPage ->
                    viewDiary model
            , case model.confirmDialog of
                Nothing ->
                    text ""

                Just dialog ->
                    viewConfirmDialog model.lang dialog
            , case model.infoDialog of
                Nothing ->
                    text ""

                Just message ->
                    viewInfoDialog model.lang message
            , if model.tagForm.open then
                viewTagPanel model.lang model.tagForm model.tagCatalog

              else
                text ""
            , if model.locationForm.open then
                viewLocationPanel model.lang model.locationForm model.locationCatalog

              else
                text ""
            , if model.trashPanelOpen then
                viewTrashPanel model.lang model.trash

              else
                text ""
            , if model.themePickerOpen then
                viewThemePicker model.lang model.themeColor

              else
                text ""
            ]
        ]
    }



-- DIALOGS -----------------------------------------------
-- | Modal confirmation dialog with action-specific message and yes/cancel buttons


viewConfirmDialog : Language -> { message : String, action : ConfirmAction } -> Html Msg
viewConfirmDialog lang dialog =
    div [ class "modal-backdrop" ]
        [ div [ class "confirm-dialog" ]
            [ p [ class "confirm-message" ] [ text (confirmMessage lang dialog.action dialog.message) ]
            , div [ class "confirm-actions" ]
                [ button [ class "btn-primary", type_ "button", onClick DoConfirm ] [ text (tr lang "Yes, delete") ]
                , button [ class "btn-ghost", type_ "button", onClick CancelConfirm ] [ text (tr lang "Cancel") ]
                ]
            ]
        ]



-- | Resolve localized confirmation text for each action; falls back to the caller-supplied string for categories


confirmMessage : Language -> ConfirmAction -> String -> String
confirmMessage lang action fallback =
    case action of
        ConfirmDeleteMemory _ ->
            tr lang "Really delete this memory?"

        ConfirmDeleteImportantDay _ ->
            tr lang "Really delete this day?"

        ConfirmDeleteNote _ ->
            tr lang "Really delete this note?"

        ConfirmDeletePlan _ ->
            tr lang "Really delete this plan?"

        ConfirmDeleteCategory _ _ ->
            tr lang fallback

        ConfirmDeleteTagFromAll tag ->
            confirmRemoveTag lang tag

        ConfirmDeleteLocationFromAll loc ->
            confirmRemovePlace lang loc

        ConfirmDeleteDiaryEntry _ ->
            tr lang "Really delete this entry?"



-- | Non-destructive info overlay shown after a failed mutation


viewInfoDialog : Language -> String -> Html Msg
viewInfoDialog lang message =
    div [ class "modal-backdrop" ]
        [ div [ class "confirm-dialog info-dialog" ]
            [ p [ class "confirm-message" ] [ text (tr lang message) ]
            , div [ class "confirm-actions" ]
                [ button [ class "btn-primary", type_ "button", onClick CloseInfoDialog ] [ text (tr lang "Got it") ]
                ]
            ]
        ]



-- SIDE PANELS -----------------------------------------------
-- | Slide-in panel for managing the global tag catalog


viewTagPanel : Language -> TagLocationForm -> List String -> Html Msg
viewTagPanel lang form allTags =
    div [ class "modal-backdrop" ]
        [ div [ class "category-panel" ]
            [ div [ class "category-panel-header" ]
                [ h3 [] [ text (tr lang "Tags") ]
                , button [ class "close-btn", type_ "button", onClick CloseTagPanel ] [ text "×" ]
                ]
            , div [ class "category-list" ]
                (List.map (viewTagRow form) allTags)
            , div [ class "category-add-row" ]
                [ input
                    [ type_ "text"
                    , placeholder (tr lang "New tag…")
                    , value form.newName
                    , onInput SetTagNewName
                    , onEnter SubmitNewTag
                    , class "category-add-input"
                    ]
                    []
                , button [ class "btn-ghost btn-sm", type_ "button", onClick SubmitNewTag ] [ text "+" ]
                ]
            ]
        ]



-- | Single tag row: shows an inline edit input when this tag is being renamed, otherwise name + action buttons


viewTagRow : TagLocationForm -> String -> Html Msg
viewTagRow form tag =
    if form.editOldName == Just tag then
        div [ class "category-row editing" ]
            [ input
                [ class "category-edit-input"
                , type_ "text"
                , value form.editNewName
                , onInput SetTagEditName
                , onEnter SubmitTagRename
                ]
                []
            , div [ class "category-row-actions" ]
                [ button [ class "btn-ghost btn-sm", type_ "button", onClick SubmitTagRename ] [ text "✓" ]
                , button [ class "btn-ghost btn-sm", type_ "button", onClick CancelEditTag ] [ text "×" ]
                ]
            ]

    else
        div [ class "category-row" ]
            [ span [ class "category-row-name" ] [ text tag ]
            , div [ class "category-row-actions" ]
                [ button [ class "btn-icon", type_ "button", onClick (StartEditTag tag) ] [ text "✏" ]
                , button
                    [ class "btn-icon btn-danger"
                    , type_ "button"
                    , onClick (RequestConfirm (ConfirmDeleteTagFromAll tag) ("Remove tag \"" ++ tag ++ "\" from all memories?"))
                    ]
                    [ text "×" ]
                ]
            ]



-- | Slide-in panel for managing the global place catalog


viewLocationPanel : Language -> TagLocationForm -> List String -> Html Msg
viewLocationPanel lang form allLocs =
    div [ class "modal-backdrop" ]
        [ div [ class "category-panel" ]
            [ div [ class "category-panel-header" ]
                [ h3 [] [ text (tr lang "Places") ]
                , button [ class "close-btn", type_ "button", onClick CloseLocationPanel ] [ text "×" ]
                ]
            , div [ class "category-list" ]
                (List.map (viewLocationRow form) allLocs)
            , div [ class "category-add-row" ]
                [ input
                    [ type_ "text"
                    , placeholder (tr lang "New place…")
                    , value form.newName
                    , onInput SetLocationNewName
                    , onEnter SubmitNewLocation
                    , class "category-add-input"
                    ]
                    []
                , button [ class "btn-ghost btn-sm", type_ "button", onClick SubmitNewLocation ] [ text "+" ]
                ]
            ]
        ]



-- | Single place row: inline edit when renaming, otherwise name + action buttons


viewLocationRow : TagLocationForm -> String -> Html Msg
viewLocationRow form loc =
    if form.editOldName == Just loc then
        div [ class "category-row editing" ]
            [ input
                [ class "category-edit-input"
                , type_ "text"
                , value form.editNewName
                , onInput SetLocationEditName
                , onEnter SubmitLocationRename
                ]
                []
            , div [ class "category-row-actions" ]
                [ button [ class "btn-ghost btn-sm", type_ "button", onClick SubmitLocationRename ] [ text "✓" ]
                , button [ class "btn-ghost btn-sm", type_ "button", onClick CancelEditLocation ] [ text "×" ]
                ]
            ]

    else
        div [ class "category-row" ]
            [ span [ class "category-row-name" ] [ text loc ]
            , div [ class "category-row-actions" ]
                [ button [ class "btn-icon", type_ "button", onClick (StartEditLocation loc) ] [ text "✏" ]
                , button
                    [ class "btn-icon btn-danger"
                    , type_ "button"
                    , onClick (RequestConfirm (ConfirmDeleteLocationFromAll loc) ("Remove place \"" ++ loc ++ "\" from all memories?"))
                    ]
                    [ text "×" ]
                ]
            ]



-- | Slide-in panel listing soft-deleted items with restore and permanent-delete actions


viewTrashPanel : Language -> Status (List TrashEntry) -> Html Msg
viewTrashPanel lang status =
    div [ class "modal-backdrop" ]
        [ div [ class "trash-panel" ]
            [ div [ class "trash-panel-header" ]
                [ h3 [] [ text (tr lang "Trash") ]
                , button [ class "close-btn", type_ "button", onClick CloseTrashPanel ] [ text "×" ]
                ]
            , div [ class "trash-panel-body" ]
                [ case status of
                    Loading ->
                        p [] [ text (tr lang "Loading…") ]

                    Failed err ->
                        p [] [ text err ]

                    Loaded [] ->
                        p [ class "trash-empty" ] [ text (tr lang "Trash is empty.") ]

                    Loaded entries ->
                        div [ class "trash-list" ] (List.map (viewTrashEntry lang) entries)
                ]
            ]
        ]



-- | Single trash row: kind badge, item name, restore and permanent-delete buttons


viewTrashEntry : Language -> TrashEntry -> Html Msg
viewTrashEntry lang entry =
    div [ class "trash-entry" ]
        [ div [ class "trash-entry-info" ]
            [ span [ class "trash-entry-kind" ] [ text (tr lang (kindLabel entry.kind)) ]
            , span [ class "trash-entry-name" ] [ text entry.name ]
            ]
        , div [ class "trash-entry-actions" ]
            [ button [ class "btn-ghost btn-sm", type_ "button", onClick (RestoreTrashItem entry.id) ] [ text (tr lang "Restore") ]
            , button [ class "btn-ghost btn-sm btn-danger", type_ "button", onClick (DeleteTrashItemPermanently entry.id) ] [ text "×" ]
            ]
        ]



-- | Human-readable label for a trash entry kind string


kindLabel : String -> String
kindLabel kind =
    case kind of
        "memory" ->
            "Memory"

        "important-day" ->
            "Important day"

        "note" ->
            "Note"

        "plan" ->
            "Plan"

        _ ->
            kind



-- NAVIGATION -----------------------------------------------
-- | Top navigation bar with page tabs, language toggle, theme picker, and trash button


viewNav : Model -> Html Msg
viewNav model =
    nav [ class "nav" ]
        [ navTab model.lang HomePage model.page (tr model.lang "Home")
        , navTab model.lang TimelinePage model.page (tr model.lang "Timeline")
        , navTab model.lang CalendarPage model.page (tr model.lang "Calendar")
        , navTab model.lang ImportantDaysPage model.page (tr model.lang "Important days")
        , navTab model.lang DiaryPage model.page (tr model.lang "Diary")
        , navTab model.lang NotesPage model.page (tr model.lang "Notes")
        , navTab model.lang PlansPage model.page (tr model.lang "Plans")
        , navTab model.lang StatsPage model.page (tr model.lang "Stats")
        , button [ class "nav-lang-btn", type_ "button", onClick ToggleLanguage, title (tr model.lang "Switch language") ] [ text (languageCode model.lang) ]
        , button [ class "nav-theme-btn", type_ "button", onClick OpenThemePicker, title (tr model.lang "Switch theme") ] [ text "🎨" ]
        , button [ class "nav-trash-btn", type_ "button", onClick OpenTrashPanel, title (tr model.lang "Trash") ] [ text "🗑" ]
        ]



-- | Single navigation anchor; receives active class when the tab's page matches the current page


navTab : Language -> Page -> Page -> String -> Html Msg
navTab lang page current label =
    a
        [ class
            (if page == current then
                "nav-tab active"

             else
                "nav-tab"
            )
        , href (pageUrlFor lang page)
        ]
        [ text label ]



-- THEME -----------------------------------------------
-- | Colour-theme picker overlay with a swatch grid


viewThemePicker : Language -> String -> Html Msg
viewThemePicker lang currentTheme =
    div [ class "modal-backdrop" ]
        [ div [ class "theme-picker" ]
            [ div [ class "theme-picker-header" ]
                [ h3 [] [ text "Colour theme" ]
                , button [ class "btn-icon", type_ "button", onClick CloseThemePicker, title (tr lang "Close") ] [ text "✕" ]
                ]
            , div [ class "theme-swatches" ]
                [ themeSwatch "theme-rose" "#d84f75" currentTheme
                , themeSwatch "theme-red" "#c0392b" currentTheme
                , themeSwatch "theme-orange" "#e06c00" currentTheme
                , themeSwatch "theme-yellow" "#c49a00" currentTheme
                , themeSwatch "theme-light-blue" "#09cdf5" currentTheme
                , themeSwatch "theme-dark-navy" "#2c405f" currentTheme
                , themeSwatch "theme-light-green" "#4a9450" currentTheme
                , themeSwatch "theme-purple" "#6a3ab0" currentTheme
                , themeSwatch "theme-brown" "#7a3914d0" currentTheme
                , themeSwatch "theme-gray" "#6b7280" currentTheme
                ]
            ]
        ]



-- | Clickable colour button; receives an active class when it is the selected theme


themeSwatch : String -> String -> String -> Html Msg
themeSwatch theme color currentTheme =
    button
        [ class
            ("theme-swatch"
                ++ (if theme == currentTheme then
                        " active"

                    else
                        ""
                   )
            )
        , type_ "button"
        , style "background" color
        , onClick (SetThemeColor theme)
        ]
        []
