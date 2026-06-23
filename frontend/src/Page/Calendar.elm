module Page.Calendar exposing (viewCalendar)

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (..)


viewCalendar : Model -> Html Msg
viewCalendar _ =
    div [ class "empty-state" ] [ text "Kalendář – připravujeme…" ]
