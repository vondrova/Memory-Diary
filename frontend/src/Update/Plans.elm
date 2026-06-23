module Update.Plans exposing (..)

{-| Update handlers for shared plans and category management
-}

import Api.Requests
    exposing
        ( createPlanCategoryReq
        , createPlanReq
        , deletePlanCategoryReq
        , deletePlanReq
        , fetchPlanCategories
        , refreshPlans
        , renamePlanCategoryReq
        , updatePlanReq
        )
import Http
import Init exposing (emptyCategoryForm, emptyPlanForm)
import Types exposing (..)
import Utils.FormUtils exposing (planToForm, setPlanField)
import Utils.Helpers exposing (httpErr, mapStatus, mutationFailure, planFormValid)



-- | Store the plans list, or record the error


gotPlans : Result Http.Error (List CouplePlan) -> Model -> ( Model, Cmd Msg )
gotPlans result model =
    case result of
        Ok ps ->
            ( { model | plans = Loaded ps }, Cmd.none )

        Err e ->
            ( { model | plans = Failed (httpErr e) }, Cmd.none )



-- | Store the category catalog, or show an error dialog


gotPlanCategories : Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
gotPlanCategories result model =
    case result of
        Ok categories ->
            ( { model | planCategories = categories }, Cmd.none )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | On success: close the form and reload, on failure: show error dialog


planCreated : Result Http.Error CouplePlan -> Model -> ( Model, Cmd Msg )
planCreated result model =
    case result of
        Ok _ ->
            ( { model | planForm = emptyPlanForm }, refreshPlans )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Same as planCreated but for PUT


planUpdated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
planUpdated result model =
    case result of
        Ok _ ->
            ( { model | planForm = emptyPlanForm }, refreshPlans )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Remove the plan from the local list, then reload


planDeleted : Int -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
planDeleted id result model =
    case result of
        Ok _ ->
            let
                updated =
                    mapStatus (List.filter (\p -> p.id /= id)) model.plans
            in
            ( { model | plans = updated }, refreshPlans )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Open a blank plan form


openNewForm : Model -> ( Model, Cmd Msg )
openNewForm model =
    ( { model | planForm = { emptyPlanForm | open = True } }, Cmd.none )



-- | Populate the form with an existing plan's data


openEditForm : CouplePlan -> Model -> ( Model, Cmd Msg )
openEditForm plan model =
    ( { model | planForm = planToForm True plan }, Cmd.none )



-- | Close and reset the plan form


closeForm : Model -> ( Model, Cmd Msg )
closeForm model =
    ( { model | planForm = emptyPlanForm }, Cmd.none )



-- | Update the category field and open the suggestion dropdown


setCategory : String -> Model -> ( Model, Cmd Msg )
setCategory v model =
    ( { model | planForm = setPlanField (\f -> { f | category = v, catSuggestOpen = True }) model.planForm }, Cmd.none )



-- | Toggle the category suggestion dropdown


toggleCategorySuggest : Model -> ( Model, Cmd Msg )
toggleCategorySuggest model =
    ( { model | planForm = setPlanField (\f -> { f | catSuggestOpen = not f.catSuggestOpen }) model.planForm }, Cmd.none )



-- | Fill the category field from the dropdown and close it


selectCategorySuggest : String -> Model -> ( Model, Cmd Msg )
selectCategorySuggest category model =
    ( { model | planForm = setPlanField (\f -> { f | category = category, catSuggestOpen = False }) model.planForm }, Cmd.none )



-- | Update the title field


setTitle : String -> Model -> ( Model, Cmd Msg )
setTitle v model =
    ( { model | planForm = setPlanField (\f -> { f | title = v }) model.planForm }, Cmd.none )



-- | Update the detail field


setDetail : String -> Model -> ( Model, Cmd Msg )
setDetail v model =
    ( { model | planForm = setPlanField (\f -> { f | detail = v }) model.planForm }, Cmd.none )



-- | Update the done checkbox


setDone : Bool -> Model -> ( Model, Cmd Msg )
setDone v model =
    ( { model | planForm = setPlanField (\f -> { f | done = v }) model.planForm }, Cmd.none )



-- | Validate required fields, then create or update


submitForm : Model -> ( Model, Cmd Msg )
submitForm model =
    if planFormValid model.planForm then
        case model.planForm.editId of
            Nothing ->
                ( model, createPlanReq model.planForm )

            Just id ->
                ( model, updatePlanReq id model.planForm )

    else
        ( { model | planForm = setPlanField (\f -> { f | showValidation = True }) model.planForm }, Cmd.none )



-- | Toggle a plan's done state and immediately save it to the API


toggleDone : CouplePlan -> Model -> ( Model, Cmd Msg )
toggleDone plan model =
    ( model, updatePlanReq plan.id (planToForm False { plan | done = not plan.done }) )



-- | Send a DELETE request for the given plan id


deletePlan : Int -> Model -> ( Model, Cmd Msg )
deletePlan id model =
    ( model, deletePlanReq id )



-- | Switch between Pending and Done tabs


setTab : PlanTab -> Model -> ( Model, Cmd Msg )
setTab tab model =
    ( { model | planTab = tab }, Cmd.none )



-- | Open the category management modal


openCategoryPanel : Model -> ( Model, Cmd Msg )
openCategoryPanel model =
    ( { model | categoryForm = { emptyCategoryForm | open = True } }, Cmd.none )



-- | Open the category modal with a specific category's inline rename form pre-filled


openCategoryPanelEdit : String -> Model -> ( Model, Cmd Msg )
openCategoryPanelEdit oldName model =
    ( { model | categoryForm = { emptyCategoryForm | open = True, editOldName = Just oldName, editNewName = oldName } }, Cmd.none )



-- | Close and reset the category modal


closeCategoryPanel : Model -> ( Model, Cmd Msg )
closeCategoryPanel model =
    ( { model | categoryForm = emptyCategoryForm }, Cmd.none )



-- | Cancel the inline category rename without saving


cancelCategoryEdit : Model -> ( Model, Cmd Msg )
cancelCategoryEdit model =
    let
        cf =
            model.categoryForm
    in
    ( { model | categoryForm = { cf | editOldName = Nothing, editNewName = "" } }, Cmd.none )



-- | Update the new-category name input


setCategoryNewName : String -> Model -> ( Model, Cmd Msg )
setCategoryNewName v model =
    let
        cf =
            model.categoryForm
    in
    ( { model | categoryForm = { cf | newName = v } }, Cmd.none )



-- | Update the rename input for the category being edited


setCategoryEditName : String -> Model -> ( Model, Cmd Msg )
setCategoryEditName v model =
    let
        cf =
            model.categoryForm
    in
    ( { model | categoryForm = { cf | editNewName = v } }, Cmd.none )



-- | Create a new category; no-op for empty names or names already in the catalog


submitNewCategory : Model -> ( Model, Cmd Msg )
submitNewCategory model =
    let
        newName =
            String.trim model.categoryForm.newName

        cf =
            model.categoryForm
    in
    if String.isEmpty newName || List.member newName model.planCategories then
        ( model, Cmd.none )

    else
        ( { model | categoryForm = { cf | newName = "" } }
        , createPlanCategoryReq newName
        )



-- | Reload the category catalog after a successful create


planCategoryCreated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
planCategoryCreated result model =
    case result of
        Ok _ ->
            ( model, fetchPlanCategories )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Save the category rename (no-op if the new name is identical or empty)


submitCategoryRename : Model -> ( Model, Cmd Msg )
submitCategoryRename model =
    let
        newName =
            String.trim model.categoryForm.editNewName

        cf =
            model.categoryForm
    in
    if String.isEmpty newName then
        ( model, Cmd.none )

    else
        case cf.editOldName of
            Nothing ->
                ( model, Cmd.none )

            Just oldName ->
                if oldName == newName then
                    ( { model | categoryForm = { cf | editOldName = Nothing, editNewName = "" } }, Cmd.none )

                else
                    ( { model
                        | categoryForm = { cf | editOldName = Nothing, editNewName = "" }
                      }
                    , renamePlanCategoryReq oldName newName
                    )



-- | Reload plans after a category rename (category name is stored on each plan row)


planCategoryRenamed : Result Http.Error () -> Model -> ( Model, Cmd Msg )
planCategoryRenamed result model =
    case result of
        Ok _ ->
            ( model, refreshPlans )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Send a DELETE request for the category; scope limits which plans are affected


deleteCategory : String -> PlanTab -> Model -> ( Model, Cmd Msg )
deleteCategory name tab model =
    let
        scope =
            case tab of
                PendingPlans ->
                    "pending"

                DonePlans ->
                    "done"
    in
    ( model, deletePlanCategoryReq name scope )



-- | Reload plans after a category delete


planCategoryDeleted : Result Http.Error () -> Model -> ( Model, Cmd Msg )
planCategoryDeleted result model =
    case result of
        Ok _ ->
            ( model, refreshPlans )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )
