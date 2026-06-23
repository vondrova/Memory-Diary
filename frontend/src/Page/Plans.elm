module Page.Plans exposing (viewPlans)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Language
import Types exposing (..)


viewPlans : Model -> Html Msg
viewPlans model =
    let
        t = Language.tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitPlan, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "new_plan") ]
            , label [] [ text (t "category") ]
            , select [ onInput SetPlanCategory ]
                [ option [ value "travel" ] [ text "✈️ Cestování" ]
                , option [ value "food" ] [ text "🍕 Jídlo" ]
                , option [ value "activity" ] [ text "🎯 Aktivita" ]
                , option [ value "other" ] [ text "📌 Jiné" ]
                ]
            , label [] [ text (t "name") ]
            , input [ type_ "text", value model.newPlanTitle, onInput SetPlanTitle, required True ] []
            , label [] [ text (t "detail") ]
            , input [ type_ "text", value model.newPlanDetail, onInput SetPlanDetail ] []
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map (viewPlan model.lang) model.plans)
        ]


viewPlan : Lang -> CouplePlan -> Html Msg
viewPlan _ plan =
    div [ class ("memory-card" ++ (if plan.done then " plan-done" else "")) ]
        [ div [ class "memory-card-header" ]
            [ h2 [ class "memory-title" ] [ text plan.title ]
            , button [ class "btn-icon", onClick (TogglePlanDone plan) ]
                [ text (if plan.done then "✅" else "⬜") ]
            ]
        , p [ class "memory-time" ] [ text plan.category ]
        , case plan.detail of
            Just d  -> p [ class "memory-desc" ] [ text d ]
            Nothing -> text ""
        ]
