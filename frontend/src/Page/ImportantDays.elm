module Page.ImportantDays exposing (viewImportantDays)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Language
import Types exposing (..)


viewImportantDays : Model -> Html Msg
viewImportantDays model =
    let
        t = Language.tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitNewDay, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "add_day") ]
            , label [] [ text (t "name") ]
            , input [ type_ "text", value model.newDayTitle, onInput SetNewDayTitle, required True ] []
            , label [] [ text (t "date") ]
            , input [ type_ "date", value model.newDayDate, onInput SetNewDayDate, required True ] []
            , label [] [ text (t "kind") ]
            , select [ onInput SetNewDayKind ]
                [ option [ value "anniversary" ] [ text (t "anniversary") ]
                , option [ value "birthday" ] [ text (t "birthday") ]
                , option [ value "other" ] [ text (t "other_kind") ]
                ]
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ]
            (List.map viewImportantDay (List.sortBy .date model.importantDays))
        ]


viewImportantDay : ImportantDay -> Html Msg
viewImportantDay day =
    div [ class "memory-card" ]
        [ h2 [ class "memory-title" ] [ text day.title ]
        , p [ class "memory-time" ] [ text (day.kind ++ "  ·  " ++ day.date) ]
        , case day.note of
            Just n  -> p [ class "memory-desc" ] [ text n ]
            Nothing -> text ""
        ]
