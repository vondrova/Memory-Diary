module Page.Diary exposing (viewDiary)

{-| Personal journal page, one tab per partner
-}

import Html exposing (Html, button, div, p, span, text, textarea)
import Html.Attributes as Attr exposing (class, placeholder, rows, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Language exposing (tr)
import Page.Home exposing (viewAvatarName)
import Types exposing (..)


-- | Page root: owner tabs, write area, and entry list
viewDiary : Model -> Html Msg
viewDiary model =
    div [ class "diary-page" ]
        [ viewOwnerTabs model
        , viewWriteArea model
        , viewBook model
        ]


-- | Left/right tab strip for switching between partners
viewOwnerTabs : Model -> Html Msg
viewOwnerTabs model =
    div [ class "diary-owner-tabs" ]
        [ ownerTabBtn model LeftDiary model.leftAvatar.name
        , ownerTabBtn model RightDiary model.rightAvatar.name
        ]


-- | Single tab button, the active tab gets an accent underline via an extra CSS class
ownerTabBtn : Model -> DiaryTab -> String -> Html Msg
ownerTabBtn model tab name =
    button
        [ class
            (if model.diaryTab == tab then
                "diary-owner-btn diary-owner-btn--active"

             else
                "diary-owner-btn"
            )
        , type_ "button"
        , onClick (SetDiaryTab tab)
        ]
        [ viewAvatarName name ]


-- | Always-visible inline form: date input (max = today), textarea, validation messages, save button
viewWriteArea : Model -> Html Msg
viewWriteArea model =
    let
        form =
            model.diaryForm

        isEditing =
            form.editId /= Nothing
    in
    div [ class "diary-write-area" ]
        [ div [ class "diary-write-meta" ]
            [ Html.input
                [ type_ "date"
                , value form.date
                , Attr.max (todayStr model.today) -- prevent the browser calendar from showing future dates
                , onInput SetDiaryDate
                , class "diary-date-input"
                ]
                []
            , if isEditing then
                button
                    [ class "diary-cancel-edit"
                    , type_ "button"
                    , onClick CloseDiaryForm
                    ]
                    [ text (tr model.lang "Cancel") ]

              else
                text ""
            ]
        , textarea
            [ class "diary-write-textarea"
            , value form.body
            , onInput SetDiaryBody
            , placeholder (tr model.lang "Write your thoughts…")
            , rows 5
            ]
            []
        , if form.showValidation then
            if form.date > todayStr model.today then
                p [ class "diary-validation" ] [ text (tr model.lang "Diary entries cannot be dated in the future.") ]

            else if String.isEmpty (String.trim form.date) then
                p [ class "diary-validation" ] [ text (tr model.lang "Please fill in the date.") ]

            else if String.isEmpty (String.trim form.body) then
                p [ class "diary-validation" ] [ text (tr model.lang "Please write something before saving.") ]

            else
                text ""

          else
            text ""
        , div [ class "diary-write-footer" ]
            [ button
                [ class "btn-primary diary-save-btn"
                , type_ "button"
                , onClick SubmitDiaryForm
                ]
                [ text
                    (if isEditing then
                        tr model.lang "Save entry"

                     else
                        tr model.lang "Save"
                    )
                ]
            ]
        ]


-- | Entry list filtered to the active owner tab, the backend returns all entries for both partners
viewBook : Model -> Html Msg
viewBook model =
    let
        owner =
            case model.diaryTab of
                LeftDiary ->
                    "left"

                RightDiary ->
                    "right"
    in
    case model.diaryEntries of
        Loading ->
            p [ class "state-msg" ] [ text (tr model.lang "Loading…") ]

        Failed e ->
            p [ class "state-msg error" ] [ text (tr model.lang "Error: " ++ e) ]

        Loaded entries ->
            let
                visible =
                    List.filter (\e -> e.owner == owner) entries
            in
            if List.isEmpty visible then
                p [ class "diary-book-empty" ] [ text (tr model.lang "No entries yet. Start writing!") ]

            else
                div [ class "diary-book" ]
                    (List.map (viewBookEntry model.lang) visible)


-- | One journal page, edit/delete action buttons are hidden via CSS and revealed on hover
viewBookEntry : Language -> DiaryEntry -> Html Msg
viewBookEntry lang entry =
    div [ class "diary-book-entry" ]
        [ div [ class "diary-book-entry-header" ]
            [ span [ class "diary-book-date" ] [ text (formatDate entry.date) ]
            , div [ class "diary-book-actions" ]
                [ button
                    [ class "btn-icon"
                    , type_ "button"
                    , onClick (OpenEditDiaryEntry entry)
                    , title (tr lang "Edit")
                    ]
                    [ text "✎" ]
                , button
                    [ class "btn-icon btn-danger"
                    , type_ "button"
                    , onClick (RequestConfirm (ConfirmDeleteDiaryEntry entry.id) (tr lang "Really delete this entry?"))
                    , title (tr lang "Delete")
                    ]
                    [ text "×" ]
                ]
            ]
        , p [ class "diary-book-body" ] [ text entry.body ]
        ]


-- | Trim a UTC timestamp like "2026-06-22T00:00:00Z" to just the date "2026-06-22"
formatDate : String -> String
formatDate iso =
    String.left 10 iso


-- | Format today as YYYY-MM-DD, used as the date input max attribute and in frontend validation
todayStr : { year : Int, month : Int, day : Int } -> String
todayStr d =
    String.fromInt d.year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt d.month)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt d.day)
