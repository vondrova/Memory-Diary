module Page.Timeline exposing (onEnter, viewMemoryCard, viewTimeline)

{-| Timeline page: memory list with filters, the CRUD form, and on-this-day band
-}

import Html exposing (Attribute, Html, a, button, datalist, div, h1, h2, h3, img, input, label, option, p, span, strong, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Language exposing (tr)
import Types exposing (..)
import Utils.FormUtils exposing (memRequiredFieldsValid, validTimeOrder)
import Utils.Helpers
    exposing
        ( formGroupClass
        , formatTimeRange
        , isBlank
        , mapsUrl
        , photoUrl
        , requiredClass
        , uniqueStrings
        , validationNotice
        )



-- | Page root: hero, on-this-day band, filter bar, optional form, and memory list


viewTimeline : Model -> Html Msg
viewTimeline model =
    div []
        [ div [ class "page-hero" ]
            [ div []
                [ p [ class "eyebrow" ] [ text "Memory Diary" ]
                , h1 [] [ text (tr model.lang "Places, people, and photos in one diary") ]
                ]
            , button [ class "btn-primary hero-action", onClick OpenNewMemForm ] [ text (tr model.lang "+ New memory") ]
            ]
        , viewOnThisDay model.lang model.onThisDay
        , viewFilterPanel model.lang model.filter model.tagCatalog model.locationCatalog
        , div [ class "timeline-header" ]
            [ h2 [] [ text (tr model.lang "Memories") ]
            , div [ class "header-actions" ]
                [ button [ class "btn-ghost btn-sm", onClick OpenTagPanel ] [ text (tr model.lang "Tags") ]
                , button [ class "btn-ghost btn-sm", onClick OpenLocationPanel ] [ text (tr model.lang "Places") ]
                ]
            ]
        , if model.memForm.open then
            viewMemForm model.lang model.memForm model.tagInput model.photoInput model.tagCatalog model.locationCatalog model.addressSuggestions

          else
            text ""
        , case model.timelineMemories of
            Loading ->
                p [ class "state-msg" ] [ text (tr model.lang "Loading…") ]

            Failed e ->
                p [ class "state-msg error" ] [ text (tr model.lang "Error: " ++ e) ]

            Loaded [] ->
                if model.filter == { q = "", tag = "", location = "", dateFrom = "", dateTo = "" } then
                    p [ class "state-msg" ] [ text (tr model.lang "No memories yet.") ]

                else
                    p [ class "state-msg" ] [ text (tr model.lang "No memories match the filter.") ]

            Loaded ms ->
                div [ class "timeline" ] (List.map (viewMemoryCard model.lang) ms)
        ]



-- | Horizontal strip of mini cards from the same day in past years, hidden when empty


viewOnThisDay : Language -> Status (List Memory) -> Html Msg
viewOnThisDay lang status =
    case status of
        Loaded [] ->
            text ""

        Loaded ms ->
            div [ class "on-this-day" ]
                [ h3 [] [ text (tr lang "On this day in history") ]
                , div [ class "otd-cards" ] (List.map (viewOtdCard lang) ms)
                ]

        _ ->
            text ""



-- | Compact card showing just the year and title of an on-this-day memory


viewOtdCard : Language -> Memory -> Html Msg
viewOtdCard lang mem =
    div [ class "otd-card" ]
        [ span [ class "otd-year" ] [ text (String.left 4 mem.timeFrom) ]
        , div [ class "otd-main" ]
            [ span [] [ text mem.title ]
            , case mem.location of
                Just loc ->
                    span [ class "otd-loc" ] [ text (" · " ++ loc) ]

                Nothing ->
                    text ""
            ]
        , div [ class "memory-actions otd-actions" ]
            [ button [ class "btn-icon", onClick (OpenEditMemForm mem), title (tr lang "Edit") ] [ text "✏" ]
            , button [ class "btn-icon btn-danger", onClick (RequestConfirm (ConfirmDeleteMemory mem.id) "Really delete this memory?"), title (tr lang "Delete") ] [ text "🗑" ]
            ]
        ]



-- | Filter bar: full-text search, tag, place, and date-range inputs with a clear button


viewFilterPanel : Language -> Filter -> List String -> List String -> Html Msg
viewFilterPanel lang f tagOptions locationOptions =
    div [ class "filter-panel" ]
        [ input [ type_ "text", placeholder (tr lang "Search…"), value f.q, onInput SetFilterQ, class "filter-input" ] []
        , input [ type_ "text", placeholder "Tag", value f.tag, onInput SetFilterTag, class "filter-input filter-short", attribute "list" "timeline-tag-options" ] []
        , datalist [ id "timeline-tag-options" ] (List.map (\tag -> option [ value tag ] []) tagOptions)
        , input [ type_ "text", placeholder (tr lang "Place"), value f.location, onInput SetFilterLocation, class "filter-input filter-short", attribute "list" "timeline-location-options" ] []
        , datalist [ id "timeline-location-options" ] (List.map (\loc -> option [ value loc ] []) locationOptions)
        , label [ class "filter-date-field" ]
            [ span [] [ text (tr lang "From") ]
            , input [ type_ "date", value f.dateFrom, onInput SetFilterFrom, class "filter-input filter-short" ] []
            ]
        , label [ class "filter-date-field" ]
            [ span [] [ text (tr lang "To") ]
            , input [ type_ "date", value f.dateTo, onInput SetFilterTo, class "filter-input filter-short" ] []
            ]
        , if f == { q = "", tag = "", location = "", dateFrom = "", dateTo = "" } then
            text ""

          else
            button [ class "btn-ghost", onClick ClearFilters ] [ text (tr lang "Clear filters") ]
        ]



-- | Full memory card: time range, location link, description, tag chips, and photo strip


viewMemoryCard : Language -> Memory -> Html Msg
viewMemoryCard lang mem =
    div [ class "memory-card" ]
        [ div [ class "memory-card-top" ]
            [ div [ class "memory-time" ] [ text (formatTimeRange mem.timeFrom mem.timeTo) ]
            , div [ class "memory-actions" ]
                [ button [ class "btn-icon", onClick (OpenEditMemForm mem), title (tr lang "Edit") ] [ text "✏" ]
                , button [ class "btn-icon btn-danger", onClick (RequestConfirm (ConfirmDeleteMemory mem.id) "Really delete this memory?"), title (tr lang "Delete") ] [ text "🗑" ]
                ]
            ]
        , h3 [] [ text mem.title ]
        , case mem.location of
            Just loc ->
                a [ class "memory-location map-link", href (mapsUrl loc), target "_blank", rel "noopener noreferrer" ]
                    [ text ("⌖ " ++ loc) ]

            Nothing ->
                text ""
        , case mem.description of
            Just desc ->
                p [ class "memory-desc" ] [ text desc ]

            Nothing ->
                text ""
        , if List.isEmpty mem.tags then
            text ""

          else
            div [ class "tag-chips" ] (List.map (\t -> span [ class "tag-chip" ] [ text ("#" ++ t) ]) mem.tags)
        , if List.isEmpty mem.photos then
            text ""

          else
            div [ class "photo-strip" ]
                (List.map
                    (\fn ->
                        a [ href (photoUrl fn), target "_blank", rel "noopener noreferrer" ]
                            [ img [ src (photoUrl fn), class "memory-photo" ] [] ]
                    )
                    mem.photos
                )
        ]



-- | Large CRUD form with tag/location autocomplete dropdowns and a photo uploader


viewMemForm : Language -> MemForm -> String -> String -> List String -> List String -> List String -> Html Msg
viewMemForm lang form tagIn photoIn availableTags storedLocs nominatimSuggestions =
    div [ class "form-card" ]
        [ div [ class "form-header" ]
            [ h2 []
                [ text
                    (if form.editId == Nothing then
                        tr lang "New memory"

                     else
                        tr lang "Edit memory"
                    )
                ]
            , button [ class "btn-icon", onClick CloseMemForm ] [ text "✕" ]
            ]
        , validationNotice lang form.showValidation (memRequiredFieldsValid form)
        , if form.showValidation && not (validTimeOrder form) then
            p [ class "validation-message" ] [ text (tr lang "The end time must be the same as or later than the start time.") ]

          else
            text ""
        , div [ class (requiredClass form.showValidation (isBlank form.title)) ]
            [ label [] [ text (tr lang "Title") ]
            , input [ type_ "text", value form.title, onInput SetTitle, placeholder (tr lang "Memory title") ] []
            ]
        , div [ class "form-row date-row required-field" ]
            [ div [ class (formGroupClass form.showValidation (isBlank form.timeFrom)) ]
                [ label [] [ text (tr lang "From") ]
                , input [ type_ "datetime-local", value form.timeFrom, onInput SetTimeFrom ] []
                ]
            , div [ class (formGroupClass form.showValidation (isBlank form.timeTo || not (validTimeOrder form))) ]
                [ label [] [ text (tr lang "To") ]
                , input [ type_ "datetime-local", value form.timeTo, onInput SetTimeTo ] []
                ]
            ]
        , div [ class "field soft-field" ]
            [ label [] [ text (tr lang "Description") ]
            , textarea [ value form.description, onInput SetDescription, placeholder (tr lang "What happened…") ] []
            ]
        , let
            currentInput =
                String.trim form.location

            filteredStored =
                if String.isEmpty currentInput then
                    []

                else
                    List.filter
                        (\l -> String.contains (String.toLower currentInput) (String.toLower l))
                        storedLocs

            visibleSuggestions =
                if form.locSuggestOpen then
                    storedLocs

                else
                    uniqueStrings (filteredStored ++ nominatimSuggestions)
          in
          div [ class "field soft-field map-field" ]
            [ label [] [ text (tr lang "Place") ]
            , div [ class "location-row" ]
                [ div [ class "suggest-field" ]
                    [ input [ type_ "text", value form.location, onInput SetLocation, placeholder (tr lang "Place name or exact address…"), onArrowDown ToggleLocSuggest ] []
                    , if List.isEmpty storedLocs then
                        text ""

                      else
                        button
                            [ class "suggest-toggle"
                            , type_ "button"
                            , onClick ToggleLocSuggest
                            , title
                                (if form.locSuggestOpen then
                                    "Collapse places"

                                 else
                                    "Expand places"
                                )
                            ]
                            [ text
                                (if form.locSuggestOpen then
                                    "▴"

                                 else
                                    "▾"
                                )
                            ]
                    , if not (List.isEmpty visibleSuggestions) then
                        div [ class "suggest-dropdown" ]
                            (List.map (\l -> div [ class "suggest-option", onClick (SelectLocSuggest l) ] [ text l ]) visibleSuggestions)

                      else
                        text ""
                    ]
                , div [ class "map-action" ]
                    [ a
                        [ class "map-preview"
                        , href
                            (if String.isEmpty (String.trim form.location) then
                                "https://www.google.com/maps"

                             else
                                mapsUrl form.location
                            )
                        , target "_blank"
                        , rel "noopener noreferrer"
                        , title (tr lang "Open map search. The map is only for finding the place; fill the address into the field manually.")
                        ]
                        [ text "⌖" ]
                    , span [ class "map-help" ] [ text (tr lang "The icon only opens map search. Fill the selected address into Place manually.") ]
                    ]
                ]
            ]
        , let
            unusedTags =
                List.filter (\t -> not (List.member t form.tags)) availableTags
          in
          div []
            [ label [] [ text (tr lang "Tags") ]
            , div [ class "tag-input-row" ]
                [ div [ class "tag-chips" ]
                    (List.map
                        (\t ->
                            span [ class "tag-chip removable" ]
                                [ text ("#" ++ t)
                                , button [ class "tag-remove", onClick (RemoveTag t) ] [ text "×" ]
                                ]
                        )
                        form.tags
                    )
                , div [ class "suggest-field tag-suggest" ]
                    [ input
                        [ type_ "text"
                        , value tagIn
                        , onInput SetTagInput
                        , placeholder (tr lang "Add tag…")
                        , onEnter AddTag
                        , class "tag-text-input"
                        ]
                        []
                    , if List.isEmpty unusedTags then
                        text ""

                      else
                        button
                            [ class "suggest-toggle"
                            , type_ "button"
                            , onClick ToggleTagSuggest
                            , title
                                (if form.tagSuggestOpen then
                                    "Collapse tags"

                                 else
                                    "Expand tags"
                                )
                            ]
                            [ text
                                (if form.tagSuggestOpen then
                                    "▴"

                                 else
                                    "▾"
                                )
                            ]
                    , if form.tagSuggestOpen && not (List.isEmpty unusedTags) then
                        div [ class "suggest-dropdown" ]
                            (List.map (\t -> div [ class "suggest-option", onClick (AddTagFromCombo t) ] [ text t ]) unusedTags)

                      else
                        text ""
                    ]
                , button [ class "btn-ghost", onClick AddTag ] [ text "+" ]
                ]
            ]
        , label [] [ text (tr lang "Photos") ]
        , div [ class "photo-dropzone", onClick PickPhotos ]
            [ div [ class "photo-drop-icon" ] [ text "+" ]
            , div []
                [ strong [] [ text (tr lang "Add photos") ]
                , p [] [ text (tr lang "They are saved into the shared photos/ folder.") ]
                ]
            ]
        , div [ class "photo-name-row" ]
            [ input
                [ type_ "text"
                , value photoIn
                , onInput SetPhotoInput
                , placeholder "file-from-photos.jpg"
                , onEnter AddPhotoByName
                ]
                []
            , button [ class "btn-ghost", type_ "button", onClick AddPhotoByName ] [ text (tr lang "Attach") ]
            ]
        , if List.isEmpty form.photos then
            text ""

          else
            div [ class "photo-preview-strip" ]
                (List.map
                    (\fn ->
                        div [ class "photo-preview-item" ]
                            [ img [ src (photoUrl fn), class "photo-thumb" ] []
                            , button [ class "btn-icon btn-danger", onClick (RemovePhoto fn) ] [ text "×" ]
                            ]
                    )
                    form.photos
                )
        , button [ class "btn-primary", onClick SubmitMemForm ]
            [ text
                (if form.editId == Nothing then
                    tr lang "Save"

                 else
                    tr lang "Update"
                )
            ]
        ]



-- | Fire a Msg when the Enter key is pressed, shared with the Plans page


onEnter : Msg -> Attribute Msg
onEnter msg =
    on "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    if key == "Enter" then
                        D.succeed msg

                    else
                        D.fail "not enter"
                )
        )



-- | Fire a Msg when the ArrowDown key is pressed, used to open location suggestions


onArrowDown : Msg -> Attribute Msg
onArrowDown msg =
    on "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    if key == "ArrowDown" then
                        D.succeed msg

                    else
                        D.fail "not arrow down"
                )
        )
