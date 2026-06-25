module Page.Trash exposing (viewTrash)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Language
import Types exposing (..)


viewTrash : Model -> Html Msg
viewTrash model =
    let
        t =
            Language.tr model.lang
    in
    div []
        [ h2 [ style "margin-bottom" "1rem" ] [ text (t "trash_title") ]
        , case model.trash of
            Loading ->
                p [] [ text (t "Loading…") ]

            Failed _ ->
                p [] [ text (t "Could not load trash.") ]

            Loaded [] ->
                p [ class "empty-state" ] [ text (t "trash_empty") ]

            Loaded entries ->
                div [ class "memory-list" ]
                    (List.map
                        (\mem ->
                            div [ class "memory-card memory-card--trashed" ]
                                [ div [ class "memory-card-header" ]
                                    [ h2 [ class "memory-title" ] [ text mem.name ]
                                    , button [ class "btn-secondary", onClick (RestoreTrashItem mem.id) ]
                                        [ text (t "restore") ]
                                    ]
                                , p [ class "memory-time" ] [ text mem.deletedAt ]
                                ]
                        )
                        entries
                    )
        ]
