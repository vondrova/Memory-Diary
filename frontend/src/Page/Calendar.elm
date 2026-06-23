module Page.Calendar exposing (viewCalendar)

{-| Calendar page with a month grid, memories, and important days
-}

import Utils.DateUtils exposing (calendarWeeks, dayKey, monthName)
import Utils.Helpers exposing (countdownShort, displayImportantDayTitle, importantDaysOnCalendarDay, memoriesOnDay)
import Html exposing (Html, button, div, h2, h3, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Language exposing (tr)
import Page.Timeline exposing (viewMemoryCard)
import Types exposing (..)


-- | Root calendar view: header with month navigation, the week grid, and the selected-day detail panel
viewCalendar : Model -> Html Msg
viewCalendar model =
    let
        mems =
            case model.memories of
                Loaded ms ->
                    ms

                _ ->
                    []

        importantDays =
            case model.importantDays of
                Loaded anns ->
                    anns

                _ ->
                    []

        weeks =
            calendarWeeks model.calYear model.calMonth -- list of weeks, each week is a list of Maybe Int (Nothing = padding cell)
    in
    div [ class "calendar-page" ]
        [ div [ class "cal-header" ]
            [ button [ class "btn-ghost", onClick PrevMonth ] [ text "‹" ]
            , div []
                [ p [ class "eyebrow" ] [ text (tr model.lang "Calendar") ]
                , h2 [] [ text (monthName model.lang model.calMonth ++ " " ++ String.fromInt model.calYear) ]
                ]
            , button [ class "btn-ghost", onClick NextMonth ] [ text "›" ]
            ]
        , div [ class "cal-grid" ]
            -- day-of-week header row followed by all day cells flattened from weeks
            (List.map (\d -> div [ class "cal-dow" ] [ text d ])
                (case model.lang of
                    English ->
                        [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]

                    Czech ->
                        [ "Po", "Út", "St", "Čt", "Pá", "So", "Ne" ]
                )
                ++ List.concatMap (List.map (viewCalDay model.lang model.today model.calYear model.calMonth mems importantDays model.selectedDay)) weeks
            )
        , case model.selectedDay of
            Nothing ->
                text ""

            Just dk ->
                let
                    dayMems =
                        memoriesOnDay mems dk

                    dayImportant =
                        importantDaysOnCalendarDay model.calYear model.calMonth dk importantDays
                in
                if List.isEmpty dayMems && List.isEmpty dayImportant then
                    p [ class "state-msg" ] [ text (tr model.lang "Nothing on this day.") ]

                else
                    div [ class "calendar-day-details" ]
                        [ if List.isEmpty dayImportant then
                            text ""

                          else
                            div [ class "calendar-day-events" ] (List.map (viewCalendarImportantDay model.lang model.today) dayImportant)
                        , if List.isEmpty dayMems then
                            text ""

                          else
                            div [ class "timeline" ] (List.map (viewMemoryCard model.lang) dayMems)
                        ]
        ]


-- | Render one cell in the month grid. Nothing = empty padding cell before the first day of the month
viewCalDay : Language -> Today -> Int -> Int -> List Memory -> List ImportantDay -> Maybe String -> Maybe Int -> Html Msg
viewCalDay lang today calY calM mems importantDays selectedDay mDay =
    case mDay of
        Nothing ->
            div [ class "cal-cell empty" ] []

        Just d ->
            let
                dk =
                    dayKey calY calM d

                dayMems =
                    memoriesOnDay mems dk

                dayImportant =
                    importantDaysOnCalendarDay calY calM dk importantDays

                hasMems =
                    not (List.isEmpty dayMems)

                hasDays =
                    not (List.isEmpty dayImportant)

                hasEvents =
                    hasMems || hasDays

                isToday =
                    calY == today.year && calM == today.month && d == today.day

                isSelected =
                    selectedDay == Just dk

                cls =
                    "cal-cell"
                        ++ (if hasEvents then
                                " has-mems"

                            else
                                ""
                           )
                        ++ (if hasDays then
                                " has-days"

                            else
                                ""
                           )
                        ++ (if isToday then
                                " today"

                            else
                                ""
                           )
                        ++ (if isSelected then
                                " selected"

                            else
                                ""
                           )
            in
            div [ class cls, onClick (SelectDay dk) ]
                [ span [ class "cal-day-number" ] [ text (String.fromInt d) ]
                , div [ class "cal-day-labels" ]
                    -- at most 2 labels per cell to avoid overflowing the grid
                    (List.take 2
                        (List.map (\ann -> span [ class "cal-day-label important" ] [ text (displayImportantDayTitle lang ann) ]) dayImportant
                            ++ List.map (\mem -> span [ class "cal-day-label memory" ] [ text mem.title ]) dayMems
                        )
                    )
                , if hasEvents then
                    span [ class "cal-dot" ] []

                  else
                    text ""
                ]


-- | Render an important day card in the selected-day detail panel
viewCalendarImportantDay : Language -> Today -> ImportantDay -> Html Msg
viewCalendarImportantDay lang _ ann =
    let
        daysLeft =
            ann.daysUntil

        countdown =
            if daysLeft == 0 then
                tr lang "Today!"

            else if daysLeft == 1 then
                tr lang "Tomorrow"

            else
                countdownShort lang daysLeft
    in
    div [ class "calendar-event-card" ]
        [ span [ class "note-person" ] [ text (tr lang "Important day") ]
        , h3 [] [ text (displayImportantDayTitle lang ann) ]
        , p [] [ text countdown ]
        , case ann.note of
            Just n ->
                p [ class "memory-desc" ] [ text n ]

            Nothing ->
                text ""
        ]
