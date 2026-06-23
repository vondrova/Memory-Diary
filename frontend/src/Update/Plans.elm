module Update.Plans exposing (updatePlans)

import Api.Requests as Requests
import Types exposing (..)


updatePlans : Msg -> Model -> ( Model, Cmd Msg )
updatePlans msg model =
    case msg of
        GotPlans (Ok ps) ->
            ( { model | plans = ps }, Cmd.none )

        GotPlans (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst plány." }, Cmd.none )

        SetPlanCategory v ->
            ( { model | newPlanCategory = v }, Cmd.none )

        SetPlanTitle v ->
            ( { model | newPlanTitle = v }, Cmd.none )

        SetPlanDetail v ->
            ( { model | newPlanDetail = v }, Cmd.none )

        SubmitPlan ->
            ( model
            , Requests.createPlan
                { category = model.newPlanCategory, title = model.newPlanTitle, detail = model.newPlanDetail }
                PlanCreated
            )

        PlanCreated (Ok plan) ->
            ( { model
                | plans = model.plans ++ [ plan ]
                , newPlanTitle = ""
                , newPlanDetail = ""
              }
            , Cmd.none
            )

        PlanCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit plán." }, Cmd.none )

        TogglePlanDone plan ->
            ( model, Requests.updatePlan { plan | done = not plan.done } PlanUpdated )

        PlanUpdated (Ok updated) ->
            ( { model | plans = List.map (\p -> if p.id == updated.id then updated else p) model.plans }
            , Cmd.none
            )

        PlanUpdated (Err _) ->
            ( { model | error = Just "Nepodařilo se aktualizovat plán." }, Cmd.none )

        _ ->
            ( model, Cmd.none )
