module Update.Notes exposing (updateNotes)

import Api.Requests as Requests
import Types exposing (..)


updateNotes : Msg -> Model -> ( Model, Cmd Msg )
updateNotes msg model =
    case msg of
        GotNotes (Ok ns) ->
            ( { model | notes = ns }, Cmd.none )

        GotNotes (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst poznámky." }, Cmd.none )

        SetNoteOwner v ->
            ( { model | newNoteOwner = v }, Cmd.none )

        SetNoteTitle v ->
            ( { model | newNoteTitle = v }, Cmd.none )

        SetNoteBody v ->
            ( { model | newNoteBody = v }, Cmd.none )

        SubmitNote ->
            ( model
            , Requests.createNote
                { owner = model.newNoteOwner, title = model.newNoteTitle, body = model.newNoteBody }
                NoteCreated
            )

        NoteCreated (Ok note) ->
            ( { model
                | notes = model.notes ++ [ note ]
                , newNoteOwner = ""
                , newNoteTitle = ""
                , newNoteBody = ""
              }
            , Cmd.none
            )

        NoteCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit poznámku." }, Cmd.none )

        _ ->
            ( model, Cmd.none )
