module Page.ImportantDays exposing (viewImportantDays)

{-| Important-days page for couple dates and reminders
-}

import Html exposing (Html, button, div, h1, h2, h3, input, label, p, small, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Language exposing (tr)
import Types exposing (..)
import Utils.DateUtils exposing (dateParts)
import Utils.Helpers
    exposing
        ( countdownShort
        , displayImportantDayTitle
        , formatDate
        , formatDateParts
        , importantDayFormValid
        , isBlank
        , requiredClass
        , validationNotice
        )



-- | Page root: relationship panel, add button, and sorted list of important days


viewImportantDays : Model -> Html Msg
viewImportantDays model =
    div [ class "milestones-page" ]
        [ div [ class "milestones-hero" ]
            [ p [ class "eyebrow" ] [ text "Love app" ]
            , h1 [] [ text (tr model.lang "Important days") ]
            ]
        , viewRelationshipPanel model.lang model.today model.relationshipDate model.relationshipValidation model.relationship model.editingRelationship
        , div [ class "timeline-header" ]
            [ h2 [] [ text (tr model.lang "Important dates") ]
            , button [ class "btn-primary", onClick OpenNewImportantDayForm ] [ text (tr model.lang "+ Add day") ]
            ]
        , if model.importantDayForm.open then
            viewImportantDayForm model.lang model.importantDayForm

          else
            text ""
        , case model.importantDays of
            Loading ->
                p [ class "state-msg" ] [ text (tr model.lang "Loading…") ]

            Failed e ->
                p [ class "state-msg error" ] [ text (tr model.lang "Error: " ++ e) ]

            Loaded as_ ->
                let
                    visible =
                        sortedImportantDays model.today as_
                in
                if List.isEmpty visible then
                    p [ class "state-msg" ] [ text (tr model.lang "No birthdays or other important days yet.") ]

                else
                    div [ class "timeline" ] (List.map (viewImportantDayCard model.lang model.today) visible)
        ]



-- | Relationship counter panel, shows stats or a prompt to set the start date


viewRelationshipPanel : Language -> Today -> String -> Bool -> Status RelationshipData -> Bool -> Html Msg
viewRelationshipPanel lang today dateInput showValidation relationship editingRelationship =
    div [ class "relationship-panel" ]
        [ div [ class "relationship-copy" ]
            [ h2 [] [ text (tr lang "Relationship counter") ]
            , case relationship of
                Loaded rel ->
                    case rel.startDate of
                        Nothing ->
                            p [] [ text (tr lang "Enter the day you got together. The app will track each monthiversary and anniversary.") ]

                        Just _ ->
                            text ""

                _ ->
                    text ""
            ]
        , case relationship of
            Loaded rel ->
                case rel.startDate of
                    Nothing ->
                        div [ class "relationship-form" ]
                            [ div [ class "relationship-empty" ] [ text (tr lang "After saving, your shared counter will appear here.") ]
                            , button [ class "btn-primary", onClick OpenRelationshipEditor ] [ text (tr lang "Set start date") ]
                            ]

                    Just _ ->
                        div [ class "relationship-content" ]
                            [ viewRelationshipStats lang today rel
                            , button [ class "btn-ghost btn-sm relationship-edit-action", onClick OpenRelationshipEditor ] [ text (tr lang "Edit") ]
                            ]

            _ ->
                div [ class "relationship-form" ]
                    [ button [ class "btn-primary", onClick OpenRelationshipEditor ] [ text (tr lang "Set start date") ] ]
        , if editingRelationship then
            viewRelationshipEditorModal lang dateInput showValidation

          else
            text ""
        ]



-- | Modal for setting the relationship start date


viewRelationshipEditorModal : Language -> String -> Bool -> Html Msg
viewRelationshipEditorModal lang dateInput showValidation =
    div [ class "modal-backdrop" ]
        [ div [ class "relationship-editor-modal" ]
            [ div [ class "form-header" ]
                [ h2 [] [ text (tr lang "Relationship start") ]
                , button [ class "btn-icon", onClick CloseRelationshipEditor ] [ text "✕" ]
                ]
            , validationNotice lang showValidation (not (isBlank dateInput))
            , div [ class (requiredClass showValidation (isBlank dateInput)) ]
                [ label [] [ text (tr lang "We got together") ]
                , input [ type_ "date", value dateInput, onInput SetRelationshipDate ] []
                ]
            , button [ class "btn-primary", style "margin-top" "0.5rem", onClick SubmitRelationship ] [ text (tr lang "Save") ]
            ]
        ]



-- | Days-together counter plus next monthiversary and anniversary


viewRelationshipStats : Language -> Today -> RelationshipData -> Html Msg
viewRelationshipStats lang _ relationship =
    let
        togetherDays =
            Maybe.withDefault 0 relationship.daysTogether
    in
    div [ class "relationship-stats" ]
        [ div [ class "love-counter primary" ]
            [ span [ class "relationship-metric-value" ] [ text (String.fromInt togetherDays) ]
            , small [ class "relationship-metric-label" ] [ text (tr lang "days together") ]
            ]
        , viewRelationshipEvent lang (tr lang "monthiversary") relationship.nextMonthiversary relationship.monthiversaryNum relationship.monthiversaryDays
        , viewRelationshipEvent lang (tr lang "anniversary") relationship.nextAnniversary relationship.anniversaryNumber relationship.anniversaryDays
        ]



-- | One relationship milestone: label, countdown, and formatted date


viewRelationshipEvent : Language -> String -> Maybe String -> Maybe Int -> Maybe Int -> Html Msg
viewRelationshipEvent lang label maybeDate maybeNumber maybeDays =
    case Maybe.andThen dateParts maybeDate of
        Nothing ->
            div [ class "love-counter relationship-event" ]
                [ small [ class "relationship-event-label" ] [ text label ]
                , span [ class "relationship-metric-value" ] [ text "—" ]
                ]

        Just date ->
            let
                daysLeft =
                    Maybe.withDefault 0 maybeDays

                numberPrefix =
                    maybeNumber
                        |> Maybe.map (\number -> String.fromInt number ++ ". ")
                        |> Maybe.withDefault ""
            in
            div [ class "love-counter relationship-event" ]
                [ small [ class "relationship-event-label" ] [ text (numberPrefix ++ label) ]
                , span [ class "relationship-metric-value" ] [ text (countdownShort lang daysLeft) ]
                , small [ class "relationship-event-date" ] [ text (formatDateParts date) ]
                ]



-- | Card for one important day: date, title, countdown, and optional note.
-- Only manual records (kind == "manual") show edit/delete buttons


viewImportantDayCard : Language -> Today -> ImportantDay -> Html Msg
viewImportantDayCard lang _ ann =
    let
        dateStr =
            formatDate ann.date

        daysLeft =
            ann.daysUntil
    in
    div [ class "memory-card ann-card" ]
        [ div [ class "memory-card-top" ]
            [ div [ class "memory-time" ] [ text dateStr ]
            , case ( ann.kind, ann.id ) of
                ( "manual", Just id ) ->
                    div [ class "memory-actions" ]
                        [ button [ class "btn-icon", onClick (OpenEditImportantDayForm ann), title (tr lang "Edit") ] [ text "✏" ]
                        , button [ class "btn-icon btn-danger", onClick (RequestConfirm (ConfirmDeleteImportantDay id) "Really delete this day?"), title (tr lang "Delete") ] [ text "🗑" ]
                        ]

                _ ->
                    text ""
            ]
        , h3 [] [ text (displayImportantDayTitle lang ann) ]
        , p [ class "ann-countdown" ] [ text (countdownShort lang daysLeft) ]
        , case ann.note of
            Just n ->
                p [ class "memory-desc" ] [ text n ]

            Nothing ->
                text ""
        ]



-- | Sort days by daysUntil so the soonest appears first


sortedImportantDays : Today -> List ImportantDay -> List ImportantDay
sortedImportantDays _ importantDays =
    List.sortBy .daysUntil importantDays



-- | Inline form for creating or editing a manual important day


viewImportantDayForm : Language -> ImportantDayForm -> Html Msg
viewImportantDayForm lang form =
    div [ class "form-card" ]
        [ div [ class "form-header" ]
            [ h2 []
                [ text
                    (if form.editId == Nothing then
                        tr lang "New important day"

                     else
                        tr lang "Edit important day"
                    )
                ]
            , button [ class "btn-icon", onClick CloseImportantDayForm ] [ text "✕" ]
            ]
        , validationNotice lang form.showValidation (importantDayFormValid form)
        , div [ class (requiredClass form.showValidation (isBlank form.title)) ]
            [ label [] [ text (tr lang "Title") ]
            , input [ type_ "text", value form.title, onInput SetImportantDayTitle, placeholder (tr lang "Child's birthday…") ] []
            ]
        , div [ class (requiredClass form.showValidation (isBlank form.date)) ]
            [ label [] [ text (tr lang "Date") ]
            , input [ type_ "date", value form.date, onInput SetImportantDayDate ] []
            ]
        , div [ class "field soft-field" ]
            [ label [] [ text (tr lang "Note") ]
            , input [ type_ "text", value form.note, onInput SetImportantDayNote, placeholder (tr lang "Reminder…") ] []
            ]
        , button [ class "btn-primary", onClick SubmitImportantDayForm ] [ text (tr lang "Save") ]
        ]
