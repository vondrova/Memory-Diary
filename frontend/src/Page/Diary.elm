module Page.Diary exposing (viewDiary)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Language
import Types exposing (..)


viewDiary : Model -> Html Msg
viewDiary model =
    let
        t = Language.tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitDiaryEntry, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "new_entry") ]
            , label [] [ text (t "date") ]
            , input [ type_ "date", value model.newDiaryDate, onInput SetDiaryDate, required True ] []
            , label [] [ text (t "mood") ]
            , select [ onInput SetDiaryMood ]
                [ option [ value "😊" ] [ text "😊 Šťastný/á" ]
                , option [ value "😐" ] [ text "😐 Neutrální" ]
                , option [ value "😢" ] [ text "😢 Smutný/á" ]
                , option [ value "😴" ] [ text "😴 Unavený/á" ]
                , option [ value "🥰" ] [ text "🥰 Zamilovaný/á" ]
                ]
            , label [] [ text (t "body") ]
            , textarea [ value model.newDiaryBody, onInput SetDiaryBody, required True, rows 5 ] []
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map viewDiaryEntry model.diaryEntries)
        ]


viewDiaryEntry : DiaryEntry -> Html Msg
viewDiaryEntry entry =
    div [ class "memory-card" ]
        [ div [ class "memory-card-header" ]
            [ h2 [ class "memory-title" ] [ text entry.date ]
            , case entry.mood of
                Just m  -> span [ class "mood-badge" ] [ text m ]
                Nothing -> text ""
            ]
        , p [ class "memory-desc" ] [ text entry.body ]
        ]
