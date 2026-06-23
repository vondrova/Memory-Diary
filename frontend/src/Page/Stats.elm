module Page.Stats exposing (viewStats)

import Html exposing (..)
import Html.Attributes exposing (class)
import Language
import Types exposing (..)


viewStats : Model -> Html Msg
viewStats model =
    let
        t = Language.tr model.lang
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
