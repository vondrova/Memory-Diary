module Page.Plans exposing (viewPlans)

{-| Plans page for the shared bucket list grouped by category.
-}

import Html exposing (Html, button, div, h1, h2, h3, input, label, p, section, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Language exposing (tr)
import Page.Timeline exposing (onEnter)
import Types exposing (..)
import Utils.Helpers exposing (isBlank, planFormValid, requiredClass, validationNotice)



-- | Page root: hero, Pending/Done tabs, optional panels, and category groups


viewPlans : Model -> Html Msg
viewPlans model =
    let
        allCategories =
            model.planCategories
    in
    div [ class "plans-page" ]
        [ div [ class "milestones-hero plans-hero" ]
            [ p [ class "eyebrow" ] [ text (tr model.lang "Shared plans") ]
            , h1 [] [ text (tr model.lang "What we'll make happen") ]
            ]
        , div [ class "timeline-header" ]
            [ h2 [] [ text (tr model.lang "Plans") ]
            , div [ class "header-actions" ]
                [ button [ class "btn-ghost", type_ "button", onClick OpenCategoryPanel ] [ text (tr model.lang "Categories") ]
                , button [ class "btn-primary", type_ "button", onClick OpenNewPlanForm ] [ text (tr model.lang "Create plan") ]
                ]
            ]
        , if model.categoryForm.open then
            viewCategoryPanel model.lang model.categoryForm allCategories

          else
            text ""
        , if model.planForm.open then
            viewPlanForm model.lang model.planForm allCategories

          else
            text ""
        , div [ class "plan-tabs" ]
            [ button
                [ class
                    (if model.planTab == PendingPlans then
                        "plan-tab active"

                     else
                        "plan-tab"
                    )
                , type_ "button"
                , onClick (SetPlanTab PendingPlans)
                ]
                [ text (tr model.lang "Pending") ]
            , button
                [ class
                    (if model.planTab == DonePlans then
                        "plan-tab active"

                     else
                        "plan-tab"
                    )
                , type_ "button"
                , onClick (SetPlanTab DonePlans)
                ]
                [ text (tr model.lang "Done") ]
            ]
        , case model.plans of
            Loading ->
                p [ class "state-msg" ] [ text (tr model.lang "Loading plans…") ]

            Failed e ->
                p [ class "state-msg error" ] [ text (tr model.lang "Error: " ++ e) ]

            Loaded ps ->
                let
                    visibleCategories =
                        case model.planTab of
                            PendingPlans ->
                                List.filter
                                    (\cat -> List.any (\p -> p.category == cat && not p.done) ps)
                                    allCategories

                            DonePlans ->
                                List.filter
                                    (\cat -> List.any (\p -> p.category == cat && p.done) ps)
                                    allCategories
                in
                if List.isEmpty visibleCategories then
                    p [ class "state-msg" ]
                        [ text
                            (case model.planTab of
                                PendingPlans ->
                                    tr model.lang "No pending plans."

                                DonePlans ->
                                    tr model.lang "No completed plans."
                            )
                        ]

                else
                    div [ class "plan-groups" ] (List.map (viewPlanGroup model.lang model.planTab ps) visibleCategories)
        ]



-- | Form for creating or editing a plan, category field has a filterable dropdown


viewPlanForm : Language -> PlanForm -> List String -> Html Msg
viewPlanForm lang form categories =
    let
        matchingCategories =
            let
                query =
                    String.toLower (String.trim form.category)
            in
            if String.isEmpty query then
                categories

            else
                List.filter (\category -> String.contains query (String.toLower category)) categories
    in
    div [ class "form-card plan-form" ]
        [ div [ class "form-header" ]
            [ h2 []
                [ text
                    (if form.editId == Nothing then
                        tr lang "New plan"

                     else
                        tr lang "Edit plan"
                    )
                ]
            , button [ class "btn-icon", onClick ClosePlanForm ] [ text "✕" ]
            ]
        , validationNotice lang form.showValidation (planFormValid form)
        , div [ class (requiredClass form.showValidation (isBlank form.category)) ]
            [ label [] [ text (tr lang "Category") ]
            , div [ class "suggest-field category-suggest" ]
                [ input [ type_ "text", value form.category, onInput SetPlanCategory, placeholder (tr lang "Bucket list, Food, Movies…") ] []
                , if List.isEmpty categories then
                    text ""

                  else
                    button
                        [ class "suggest-toggle"
                        , type_ "button"
                        , onClick TogglePlanCategorySuggest
                        , title
                            (if form.catSuggestOpen then
                                "Collapse categories"

                             else
                                "Expand categories"
                            )
                        ]
                        [ text
                            (if form.catSuggestOpen then
                                "▴"

                             else
                                "▾"
                            )
                        ]
                , if form.catSuggestOpen && not (List.isEmpty matchingCategories) then
                    div [ class "suggest-dropdown" ]
                        (List.map (\category -> div [ class "suggest-option", onClick (SelectPlanCategorySuggest category) ] [ text category ]) matchingCategories)

                  else
                    text ""
                ]
            ]
        , div [ class (requiredClass form.showValidation (isBlank form.title)) ]
            [ label [] [ text (tr lang "Title") ]
            , input [ type_ "text", value form.title, onInput SetPlanTitle, placeholder (tr lang "Sushi night, cinema, trip…") ] []
            ]
        , div [ class "field soft-field" ]
            [ label [] [ text (tr lang "Detail") ]
            , textarea [ value form.detail, onInput SetPlanDetail, placeholder (tr lang "Address, note, link, why we want it…") ] []
            ]
        , label [ class "checkbox-row" ]
            [ input [ type_ "checkbox", checked form.done, onCheck SetPlanDone ] []
            , span [] [ text (tr lang "Done") ]
            ]
        , button [ class "btn-primary", onClick SubmitPlanForm ] [ text (tr lang "Save") ]
        ]



-- | Section for one category with a header, count badge, and plan card grid


viewPlanGroup : Language -> PlanTab -> List CouplePlan -> String -> Html Msg
viewPlanGroup lang tab plans categoryName =
    let
        inCategory =
            List.filter (\plan -> plan.category == categoryName) plans

        tabFiltered =
            case tab of
                PendingPlans ->
                    List.filter (\p -> not p.done) inCategory

                DonePlans ->
                    List.filter (\p -> p.done) inCategory

        count =
            List.length tabFiltered

        emptyMsg =
            case tab of
                PendingPlans ->
                    if List.isEmpty inCategory then
                        tr lang "Still empty. Add a plan with the Create plan button."

                    else
                        tr lang "All plans in this category are done."

                DonePlans ->
                    tr lang "No completed plans."
    in
    section [ class "plan-group" ]
        [ div [ class "panel-title-row" ]
            [ h2 [] [ text categoryName ]
            , div [ class "category-header-actions" ]
                [ span [] [ text (String.fromInt count) ]
                , button [ class "btn-icon", type_ "button", onClick (OpenCategoryPanelEdit categoryName), title (tr lang "Rename") ] [ text "✎" ]
                , button [ class "btn-icon btn-danger", type_ "button", onClick (RequestConfirm (ConfirmDeleteCategory categoryName tab) "Really delete this category?"), title (tr lang "Delete category") ] [ text "×" ]
                ]
            ]
        , if List.isEmpty tabFiltered then
            p [ class "panel-empty" ] [ text emptyMsg ]

          else
            div [ class "plan-grid" ] (List.map (viewPlanCard lang) tabFiltered)
        ]



-- | Modal for creating, renaming, and deleting categories


viewCategoryPanel : Language -> CategoryForm -> List String -> Html Msg
viewCategoryPanel lang form categories =
    let
        sorted =
            List.sortWith (\a b -> compare (String.toLower a) (String.toLower b)) categories
    in
    div [ class "modal-backdrop" ]
        [ div [ class "category-panel" ]
            [ div [ class "form-header" ]
                [ h2 [] [ text (tr lang "Categories") ]
                , button [ class "btn-icon", type_ "button", onClick CloseCategoryPanel ] [ text "✕" ]
                ]
            , div [ class "category-list" ]
                (List.map (viewCategoryRow lang form) sorted)
            , div [ class "category-add-row" ]
                [ input
                    [ type_ "text"
                    , value form.newName
                    , onInput SetCategoryNewName
                    , onEnter SubmitNewCategory
                    , placeholder (tr lang "New category…")
                    ]
                    []
                , button
                    [ class "btn-ghost"
                    , type_ "button"
                    , disabled (String.isEmpty (String.trim form.newName))
                    , onClick SubmitNewCategory
                    ]
                    [ text (tr lang "Add") ]
                ]
            ]
        ]



-- | One row in the category panel, switches to an inline text input while editing


viewCategoryRow : Language -> CategoryForm -> String -> Html Msg
viewCategoryRow lang form categoryName =
    if form.editOldName == Just categoryName then
        div [ class "category-row editing" ]
            [ input
                [ type_ "text"
                , value form.editNewName
                , onInput SetCategoryEditName
                , onEnter SubmitCategoryRename
                ]
                []
            , button
                [ class "btn-icon"
                , type_ "button"
                , disabled (String.isEmpty (String.trim form.editNewName))
                , onClick SubmitCategoryRename
                , title (tr lang "Save")
                ]
                [ text "✓" ]
            , button
                [ class "btn-icon"
                , type_ "button"
                , onClick CancelCategoryEdit
                , title (tr lang "Cancel")
                ]
                [ text "✕" ]
            ]

    else
        div [ class "category-row" ]
            [ span [] [ text categoryName ]
            , div [ class "category-row-actions" ]
                [ button
                    [ class "btn-icon"
                    , type_ "button"
                    , onClick (OpenCategoryPanelEdit categoryName)
                    , title (tr lang "Rename")
                    ]
                    [ text "✎" ]
                , button
                    [ class "btn-icon btn-danger"
                    , type_ "button"
                    , onClick (RequestConfirm (ConfirmDeleteCategory categoryName PendingPlans) "Really delete this category? Completed plans will stay.")
                    , title (tr lang "Delete category (completed plans stay)")
                    ]
                    [ text "×" ]
                ]
            ]



-- | Card for one plan: done badge, title, optional detail, toggle, edit, and delete


viewPlanCard : Language -> CouplePlan -> Html Msg
viewPlanCard lang plan =
    div
        [ class
            (if plan.done then
                "plan-card done"

             else
                "plan-card"
            )
        ]
        [ div [ class "memory-card-top" ]
            [ span [ class "note-person" ]
                [ text
                    (tr lang
                        (if plan.done then
                            "done"

                         else
                            "pending"
                        )
                    )
                ]
            , div [ class "memory-actions" ]
                [ button [ class "btn-icon", onClick (TogglePlanDone plan), title (tr lang "Undo") ]
                    [ text
                        (if plan.done then
                            "↺"

                         else
                            "✓"
                        )
                    ]
                , button [ class "btn-icon", onClick (OpenEditPlanForm plan), title (tr lang "Edit") ] [ text "✎" ]
                , button [ class "btn-icon btn-danger", onClick (RequestConfirm (ConfirmDeletePlan plan.id) "Really delete this plan?"), title (tr lang "Delete") ] [ text "×" ]
                ]
            ]
        , h3 [] [ text plan.title ]
        , case plan.detail of
            Just detail ->
                p [] [ text detail ]

            Nothing ->
                text ""
        ]
