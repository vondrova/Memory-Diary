module Page.Notes exposing (viewNotes)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Language
import Types exposing (..)


viewNotes : Model -> Html Msg
viewNotes model =
    let
        t = Language.tr model.lang
    in
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitNote, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text (t "new_note") ]
            , label [] [ text (t "from_who") ]
            , input [ type_ "text", value model.newNoteOwner, onInput SetNoteOwner, required True ] []
            , label [] [ text (t "title") ]
            , input [ type_ "text", value model.newNoteTitle, onInput SetNoteTitle, required True ] []
            , label [] [ text (t "body") ]
            , textarea [ value model.newNoteBody, onInput SetNoteBody, required True ] []
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text (t "save") ] ]
            ]
        , div [ class "memory-list" ] (List.map viewNote model.notes)
        ]


viewNote : CoupleNote -> Html Msg
viewNote note =
    div [ class "memory-card" ]
        [ h2 [ class "memory-title" ] [ text note.title ]
        , p [ class "memory-time" ] [ text (note.owner ++ "  ·  " ++ note.createdAt) ]
        , p [ class "memory-desc" ] [ text note.body ]
        ]
