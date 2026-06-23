module Page.Home exposing (viewHome, viewForm, viewMemory)

import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Language
import Types exposing (..)


viewHome : Model -> Html Msg
viewHome model =
    let
        t = Language.tr model.lang
    in
    div []
        [ div [ class "page-actions" ]
            [ input [ type_ "search", placeholder (t "search"), value model.searchQuery, onInput SetSearch, class "search-input" ] []
            , button [ class "btn-primary", onClick OpenForm ] [ text (t "add_memory") ]
            ]
        , if model.formOpen then viewForm model else text ""
        , if List.isEmpty model.memories then
            p [ class "empty-state" ] [ text (t "empty") ]
          else
            div [ class "memory-list" ] (List.map (viewMemory model.lang) model.memories)
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        t = Language.tr model.lang
    in
    div [ class "form-overlay" ]
        [ Html.form [ class "memory-form", onSubmit SubmitForm ]
            [ h2 [] [ text (t "new_memory") ]
            , label [] [ text (t "name") ]
            , input [ type_ "text", value model.formTitle, onInput SetTitle, required True ] []
            , label [] [ text (t "from") ]
            , input [ type_ "datetime-local", value model.formTimeFrom, onInput SetTimeFrom, required True ] []
            , label [] [ text (t "to") ]
            , input [ type_ "datetime-local", value model.formTimeTo, onInput SetTimeTo, required True ] []
            , label [] [ text (t "place") ]
            , div [ class "geo-row" ]
                [ input [ type_ "text", value model.formLocation, onInput SetLocation, placeholder (t "place") ] []
                , input [ type_ "text", value model.geoQuery, onInput SetGeoQuery, placeholder (t "map_search"), class "geo-input" ] []
                , button [ type_ "button", class "btn-secondary", onClick SearchGeo ] [ text "🔍" ]
                ]
            , if List.isEmpty model.geoSuggestions then text ""
              else
                div [ class "geo-suggestions" ]
                    (List.map
                        (\s -> div [ class "geo-suggestion", onClick (SelectGeoSuggestion s.displayName) ] [ text s.displayName ])
                        model.geoSuggestions)
            , label [] [ text (t "tags") ]
            , input [ type_ "text", value model.formTags, onInput SetTags ] []
            , label [] [ text (t "description") ]
            , textarea [ value model.formDescription, onInput SetDescription ] []
            , label [] [ text "Foto" ]
            , div [ class "photo-row" ]
                [ button [ type_ "button", class "btn-secondary", onClick PickPhoto ] [ text (t "pick_photo") ]
                , case model.formPendingPhoto of
                    Just f  -> span [ class "photo-name" ] [ text (File.name f) ]
                    Nothing -> text ""
                ]
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ]
                , button [ type_ "button", class "btn-secondary", onClick CloseForm ] [ text (t "cancel") ]
                ]
            ]
        ]


viewMemory : Lang -> Memory -> Html Msg
viewMemory lang mem =
    div [ class "memory-card" ]
        [ div [ class "memory-card-header" ]
            [ h2 [ class "memory-title" ] [ text mem.title ]
            , button [ class "btn-icon btn-delete", onClick (DeleteMemory mem.id) ] [ text "🗑" ]
            ]
        , p [ class "memory-time" ] [ text (mem.timeFrom ++ " – " ++ mem.timeTo) ]
        , case mem.location of
            Just loc -> p [ class "memory-location" ] [ text ("📍 " ++ loc) ]
            Nothing  -> text ""
        , case mem.description of
            Just desc -> p [ class "memory-desc" ] [ text desc ]
            Nothing   -> text ""
        , div [ class "memory-tags" ]
            (List.map (\tag -> span [ class "tag" ] [ text tag ]) mem.tags)
        ]
