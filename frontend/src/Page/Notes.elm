module Page.Notes exposing (viewNotes)

{-| Notes page for per-person and shared notebook entries.
-}

import Utils.FormUtils exposing (noteTabOwner)
import Utils.Helpers exposing (isBlank, noteFormValid, requiredClass, validationNotice)
import Html exposing (Html, aside, button, div, h1, h2, h3, input, label, option, p, select, span, strong, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Language exposing (tr)
import Page.Home exposing (viewAvatarName, viewAvatarVisual)
import Types exposing (..)


-- | Page root: hero, tabs, search bar, form, and note grid
viewNotes : Model -> Html Msg
viewNotes model =
    div [ class "notes-page" ]
        [ div [ class "milestones-hero notes-hero" ]
            [ p [ class "eyebrow" ] [ text (tr model.lang "Notebook") ]
            , h1 [] [ text (tr model.lang "Things worth remembering") ]
            ]
        , viewNoteTabs model
        , div [ class "timeline-header" ]
            [ h2 [] [ viewNoteTabLabel model model.noteTab False ]
            , button
                [ class "btn-primary"
                , onClick OpenNewNoteForm
                , disabled (not (canCreateNoteForTab model model.noteTab))
                ]
                [ text (tr model.lang "+ Add note") ]
            ]
        , div [ class "note-searchbar" ]
            [ input
                [ type_ "search"
                , class "filter-input note-search-input"
                , value model.noteSearch
                , onInput SetNoteSearch
                , placeholder (tr model.lang "Search notes…")
                ]
                []
            ]
        , if model.noteForm.open then
            viewNoteForm model model.noteForm

          else
            text ""
        , case model.notes of
            Loading ->
                p [ class "state-msg" ] [ text (tr model.lang "Loading notes…") ]

            Failed e ->
                p [ class "state-msg error" ] [ text (tr model.lang "Error: " ++ e) ]

            Loaded notes ->
                let
                    visibleNotes =
                        notes
                            |> List.filter (noteMatchesTab model model.noteTab)
                            |> List.filter (noteMatchesSearch model.noteSearch)

                    noteList =
                        if List.isEmpty visibleNotes then
                            div [ class "notes-empty" ]
                                [ h3 [] [ text (tr model.lang "Nothing here yet") ]
                                , p [] [ text (tr model.lang (emptyNoteText model.noteTab)) ]
                                ]

                        else
                            div [ class "notes-grid" ] (List.map (viewNoteCard model) visibleNotes)
                in
                div [ class "notes-section" ]
                    [ div [] [ noteList ]
                    , viewNoteSectionAside model model.noteTab
                    ]
        ]


-- | Three-tab strip for shared, left, and right notes
viewNoteTabs : Model -> Html Msg
viewNoteTabs model =
    div [ class "plan-tabs note-tabs-simple" ]
        [ viewNoteTabButton model SharedNotes
        , viewNoteTabButton model LeftAvatarNotes
        , viewNoteTabButton model RightAvatarNotes
        ]


-- | Single tab button, active tab gets the active class and shows the short label
viewNoteTabButton : Model -> NoteTab -> Html Msg
viewNoteTabButton model tab =
    button
        [ class
            (if model.noteTab == tab then
                "plan-tab active"

             else
                "plan-tab"
            )
        , type_ "button"
        , onClick (SetNoteTab tab)
        ]
        [ viewNoteTabLabel model tab True ]


-- | Sidebar with avatar visual and section description
viewNoteSectionAside : Model -> NoteTab -> Html Msg
viewNoteSectionAside model tab =
    aside [ class "note-section-aside" ]
        [ div [ class "note-section-visual" ]
            [ case tab of
                SharedNotes ->
                    span [ class "note-section-heart" ] [ text "♥" ]

                LeftAvatarNotes ->
                    viewAvatarVisual model.leftAvatar

                RightAvatarNotes ->
                    viewAvatarVisual model.rightAvatar
            ]
        , h3 [] [ viewNoteTabLabel model tab False ]
        , case tab of
            SharedNotes ->
                p [] [ text (tr model.lang (noteTabSubtitle model tab)) ]

            _ ->
                text ""
        ]


-- | Tab label: avatar name for personal tabs, "Shared" for the shared tab
viewNoteTabLabel : Model -> NoteTab -> Bool -> Html Msg
viewNoteTabLabel model tab short =
    case tab of
        SharedNotes ->
            text
                (tr model.lang
                    (if short then
                        "Shared"

                     else
                        "Shared notes"
                    )
                )

        LeftAvatarNotes ->
            viewAvatarName model.leftAvatar.name

        RightAvatarNotes ->
            viewAvatarName model.rightAvatar.name


-- | Subtitle text shown in the shared notes aside panel
noteTabSubtitle : Model -> NoteTab -> String
noteTabSubtitle model tab =
    case tab of
        SharedNotes ->
            "what applies to both of us"

        LeftAvatarNotes ->
            ""

        RightAvatarNotes ->
            ""


-- | True if the note belongs to the currently active tab
noteMatchesTab : Model -> NoteTab -> CoupleNote -> Bool
noteMatchesTab model tab note =
    note.owner == noteTabOwner tab


-- | Case-insensitive substring match against note title and body
noteMatchesSearch : String -> CoupleNote -> Bool
noteMatchesSearch query note =
    let
        q =
            String.toLower (String.trim query)
    in
    if String.isEmpty q then
        True

    else
        String.contains q (String.toLower note.title)
            || String.contains q (String.toLower note.body)


-- | Disable the add button for personal tabs when the avatar has no name yet
canCreateNoteForTab : Model -> NoteTab -> Bool
canCreateNoteForTab model tab =
    case tab of
        SharedNotes ->
            True

        LeftAvatarNotes ->
            not (String.isEmpty (String.trim model.leftAvatar.name))

        RightAvatarNotes ->
            not (String.isEmpty (String.trim model.rightAvatar.name))


-- | Placeholder text shown when a tab has no notes
emptyNoteText : NoteTab -> String
emptyNoteText tab =
    case tab of
        SharedNotes ->
            "A good place for shared things: traditions, gift ideas, home routines, or anything that belongs to both of us."

        LeftAvatarNotes ->
            "This can hold allergies, favorite colors, sizes, comfort movies, or small tips for making them happy."

        RightAvatarNotes ->
            "This can hold allergies, favorite colors, sizes, comfort movies, or small tips for making them happy."


-- | Inline form for creating or editing a note
-- Owner options are filtered to avatars that have a name, shared is always available
viewNoteForm : Model -> NoteForm -> Html Msg
viewNoteForm model form =
    let
        namedOwners =
            [ ( "left", String.trim model.leftAvatar.name )
            , ( "right", String.trim model.rightAvatar.name )
            ]
                |> List.filter (not << String.isEmpty << Tuple.second)

        ownerOptions =
            ( "shared", tr model.lang "Shared" ) :: namedOwners

        options =
            if List.any (\( owner, _ ) -> owner == form.owner) ownerOptions then
                ownerOptions

            else
                ownerOptions ++ [ ( form.owner, form.owner ) ]
    in
    div [ class "form-card note-form" ]
        [ div [ class "form-header" ]
            [ h2 []
                [ text
                    (if form.editId == Nothing then
                        tr model.lang "New note"

                     else
                        tr model.lang "Edit note"
                    )
                ]
            , button [ class "btn-icon", onClick CloseNoteForm ] [ text "✕" ]
            ]
        , validationNotice model.lang form.showValidation (noteFormValid form)
        , div [ class (requiredClass form.showValidation (not (List.member form.owner [ "shared", "left", "right" ]))) ]
            [ label [] [ text (tr model.lang "Belongs to") ]
            , select [ value form.owner, onInput SetNoteOwner ]
                (List.map (\( owner, caption ) -> option [ value owner ] [ text caption ]) options)
            ]
        , div [ class (requiredClass form.showValidation (isBlank form.title)) ]
            [ label [] [ text (tr model.lang "Title") ]
            , input [ type_ "text", value form.title, onInput SetNoteTitle, placeholder (tr model.lang "Favorite color, allergies, address…") ] []
            ]
        , div [ class (requiredClass form.showValidation (isBlank form.body)) ]
            [ label [] [ text (tr model.lang "Note") ]
            , textarea [ value form.body, onInput SetNoteBody, placeholder (tr model.lang "What do you want to remember…") ] []
            ]
        , button [ class "btn-primary", onClick SubmitNoteForm ] [ text (tr model.lang "Save") ]
        ]


-- | Note card with owner badge, title, body, and edit/delete action buttons
viewNoteCard : Model -> CoupleNote -> Html Msg
viewNoteCard model note =
    div [ class "note-card" ]
        [ div [ class "memory-card-top" ]
            [ span [ class "note-person" ] [ viewNoteOwner model note.owner ]
            , div [ class "memory-actions" ]
                [ button [ class "btn-icon", onClick (OpenEditNoteForm note), title (tr model.lang "Edit") ] [ text "✎" ]
                , button [ class "btn-icon btn-danger", onClick (RequestConfirm (ConfirmDeleteNote note.id) "Really delete this note?"), title (tr model.lang "Delete") ] [ text "×" ]
                ]
            ]
        , h3 [] [ text note.title ]
        , p [] [ text note.body ]
        ]


-- | Avatar name or "Shared" label for the owner field in a note card
viewNoteOwner : Model -> String -> Html Msg
viewNoteOwner model owner =
    case owner of
        "left" ->
            viewAvatarName model.leftAvatar.name

        "right" ->
            viewAvatarName model.rightAvatar.name

        _ ->
            text (tr model.lang "Shared")
