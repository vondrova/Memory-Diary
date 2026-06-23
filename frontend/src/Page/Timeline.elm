module Page.Timeline exposing (viewTimeline)

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (..)


viewTimeline : Model -> Html Msg
viewTimeline model =
    div [ class "timeline" ] (List.map viewTimelineItem model.memories)


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
