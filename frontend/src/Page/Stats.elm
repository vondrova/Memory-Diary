module Page.Stats exposing (viewStats)

{-| Stats page: aggregated memory data computed by the backend at request time.
-}

import Dict
import Utils.DateUtils exposing (dayKey, fromJulian, mondayFirst, toJulian)
import Utils.Helpers exposing (formatDuration)
import Html exposing (Html, div, h1, h3, p, small, span, strong, text)
import Html.Attributes exposing (..)
import Language exposing (tr)
import Types exposing (..)


-- | Page root: metric grid, insight grid, monthly bar chart, and year heatmap
viewStats : Model -> Html Msg
viewStats model =
    case model.stats of
        Loading ->
            p [ class "state-msg" ] [ text (tr model.lang "Loading stats…") ]

        Failed _ ->
            p [ class "state-msg error" ] [ text (tr model.lang "Error: ") ]

        Loaded s ->
            div [ class "stats-page love-stats" ]
                [ div [ class "stats-hero-card" ]
                    [ p [ class "eyebrow" ] [ text (tr model.lang "Stats") ]
                    , h1 [] [ text (tr model.lang "What our shared archive looks like") ]
                    ]
                , div [ class "stats-metric-grid" ]
                    [ viewMetricCard (tr model.lang "Recorded time") (formatDuration s.totalMinutes) (tr model.lang "sum of all memories")
                    , viewMetricCard (tr model.lang "Memory count") (String.fromInt s.memoryCount) (tr model.lang "saved in the timeline")
                    , viewMetricCard (tr model.lang "Visited places") (String.fromInt s.visitedPlaces) (tr model.lang "unique places")
                    , viewMetricCard (tr model.lang "Photo count") (String.fromInt s.photoCount) (tr model.lang "attached to memories")
                    ]
                , div [ class "stats-insight-grid" ]
                    [ viewStatsInsight (tr model.lang "Average length") (formatDuration s.averageMinutes) (tr model.lang "typical length of one saved memory")
                    , viewStatsInsight (tr model.lang "Longest memory") (longestLabel model.lang s) (tr model.lang "largest time span in the timeline")
                    , viewStatsInsight (tr model.lang "Top tag") (topPairLabel (tr model.lang "no tag yet") s.topTags) (tr model.lang "your most common shared theme")
                    , viewStatsInsight (tr model.lang "Top place") (topPairLabel (tr model.lang "no place yet") s.topLocations) (tr model.lang "where you return most often")
                    ]
                , div [ class "stats-card stats-wide" ]
                    [ h3 [] [ text (tr model.lang "Activity by month") ]
                    , if List.isEmpty s.monthlyActivity then
                        p [ class "state-msg" ] [ text (tr model.lang "Once you add your first memory, a monthly overview will appear here.") ]

                      else
                        viewBarChart s.monthlyActivity
                    ]
                , div [ class "stats-card stats-wide" ]
                    [ h3 [] [ text (tr model.lang "Activity heatmap") ]
                    , viewHeatmap model.lang model.today s.dailyActivity
                    ]
                ]


-- | Format the longest memory as "title · duration", or a language fallback when none exist
longestLabel : Language -> Stats -> String
longestLabel lang s =
    case s.longestTitle of
        Nothing ->
            case lang of
                English ->
                    "none yet"

                Czech ->
                    "zatím žádná"

        Just title ->
            title ++ " · " ++ formatDuration s.longestMinutes


-- | Format the top-ranked item as "name · count×", or a fallback when the list is empty
topPairLabel : String -> List ( String, Int ) -> String
topPairLabel fallback pairs =
    pairs
        |> List.head
        |> Maybe.map (\( name, count ) -> name ++ " · " ++ String.fromInt count ++ "x")
        |> Maybe.withDefault fallback


-- | Large metric tile: label above, big value, and small hint below
viewMetricCard : String -> String -> String -> Html Msg
viewMetricCard label value hint =
    div [ class "stats-card metric-card" ]
        [ span [] [ text label ]
        , strong [] [ text value ]
        , small [] [ text hint ]
        ]


-- | Insight tile with a prominent value and a hint line below
viewStatsInsight : String -> String -> String -> Html Msg
viewStatsInsight label value hint =
    div [ class "stats-card insight-card" ]
        [ h3 [] [ text label ]
        , p [ class "stats-big" ] [ text value ]
        , small [] [ text hint ]
        ]


-- | Horizontal bar chart where each bar's width is proportional to the maximum count
viewBarChart : List ( String, Int ) -> Html Msg
viewBarChart items =
    let
        maxCount =
            List.maximum (List.map Tuple.second items) |> Maybe.withDefault 1
    in
    div [ class "bar-chart" ]
        (List.map
            (\( name, count ) ->
                div [ class "bar-row" ]
                    [ span [ class "bar-label" ] [ text name ]
                    , div [ class "bar-track" ]
                        [ div
                            [ class "bar-fill"
                            , style "width"
                                (String.fromFloat (toFloat count / toFloat maxCount * 100) ++ "%")
                            ]
                            []
                        ]
                    , span [ class "bar-count" ] [ text (String.fromInt count) ]
                    ]
            )
            items
        )


-- | GitHub-style heatmap showing the last ~53 weeks, columns start on Monday
viewHeatmap : Language -> Today -> List ( String, Int ) -> Html Msg
viewHeatmap lang today dailyActivity =
    let
        countDict =
            Dict.fromList dailyActivity

        todayJDN =
            toJulian today.year today.month today.day

        rawStart =
            todayJDN - 363

        rawStartD =
            fromJulian rawStart

        -- mondayFirst returns 0=Mon … 6=Sun; shift start back to nearest Monday
        offset =
            mondayFirst rawStartD.year rawStartD.month rawStartD.day

        startJDN =
            rawStart - offset

        numWeeks =
            53

        dayIndices =
            List.range 0 6

        cellForJDN jdn =
            if jdn > todayJDN then
                div [ class "heat-cell heat-future" ] []

            else
                let
                    d =
                        fromJulian jdn

                    key =
                        dayKey d.year d.month d.day

                    count =
                        Dict.get key countDict |> Maybe.withDefault 0

                    lvl =
                        if count == 0 then
                            "0"

                        else if count == 1 then
                            "1"

                        else if count == 2 then
                            "2"

                        else if count <= 4 then
                            "3"

                        else
                            "4"
                in
                div
                    [ class ("heat-cell heat-" ++ lvl)
                    , title (key ++ if count > 0 then " · " ++ String.fromInt count else "")
                    ]
                    []

        weekColumn weekIdx =
            let
                weekJDN =
                    startJDN + weekIdx * 7

                d =
                    fromJulian weekJDN

                prevD =
                    fromJulian (weekJDN - 7)

                monthLabel =
                    if d.month /= prevD.month && weekJDN <= todayJDN then
                        span [ class "heat-month" ] [ text (heatMonthShort lang d.month) ]

                    else
                        span [ class "heat-month" ] []
            in
            div [ class "heatmap-col" ]
                (monthLabel
                    :: List.map (\di -> cellForJDN (weekJDN + di)) dayIndices
                )
    in
    div [ class "heatmap-outer" ]
        [ div [ class "heatmap-grid" ]
            (List.map weekColumn (List.range 0 (numWeeks - 1)))
        ]


-- | Three-letter month abbreviation for the heatmap column header, localized for supported languages
heatMonthShort : Language -> Int -> String
heatMonthShort lang m =
    case lang of
        Czech ->
            case m of
                1 -> "Led"
                2 -> "Úno"
                3 -> "Bře"
                4 -> "Dub"
                5 -> "Kvě"
                6 -> "Čvn"
                7 -> "Čvc"
                8 -> "Srp"
                9 -> "Zář"
                10 -> "Říj"
                11 -> "Lis"
                _ -> "Pro"

        English ->
            case m of
                1 -> "Jan"
                2 -> "Feb"
                3 -> "Mar"
                4 -> "Apr"
                5 -> "May"
                6 -> "Jun"
                7 -> "Jul"
                8 -> "Aug"
                9 -> "Sep"
                10 -> "Oct"
                11 -> "Nov"
                _ -> "Dec"
