module Update.Notes exposing (..)

{-| Update handlers for couple notes
-}

import Api.Requests exposing (createNoteReq, deleteNoteReq, refreshNotes, updateNoteReq)
import Http
import Init exposing (emptyNoteForm)
import Types exposing (..)
import Utils.FormUtils exposing (noteTabOwner, noteTitleExists, setNoteField)
import Utils.Helpers exposing (httpErr, mapStatus, noteFormValid, withMutationFailure)



-- | Store the note list from the API, or record the error


gotNotes : Result Http.Error (List CoupleNote) -> Model -> ( Model, Cmd Msg )
gotNotes result model =
    case result of
        Ok ns ->
            ( { model | notes = Loaded ns }, Cmd.none )

        Err e ->
            ( { model | notes = Failed (httpErr e) }, Cmd.none )



-- | On success: close the form and reload, on failure: show error dialog


noteCreated : Result Http.Error CoupleNote -> Model -> ( Model, Cmd Msg )
noteCreated result model =
    case result of
        Ok _ ->
            ( { model | noteForm = emptyNoteForm }, refreshNotes )

        Err error ->
            ( withMutationFailure error model, Cmd.none )



-- | Same as noteCreated but for PUT


noteUpdated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
noteUpdated result model =
    case result of
        Ok _ ->
            ( { model | noteForm = emptyNoteForm }, refreshNotes )

        Err error ->
            ( withMutationFailure error model, Cmd.none )



-- | Optimistically remove the note from the local list, then reload


noteDeleted : Int -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
noteDeleted id result model =
    case result of
        Ok _ ->
            let
                updated =
                    mapStatus (List.filter (\n -> n.id /= id)) model.notes
            in
            ( { model | notes = updated }, refreshNotes )

        Err error ->
            ( withMutationFailure error model, Cmd.none )



-- | Open a blank form pre-filled with the current tab's owner


openNewForm : Model -> ( Model, Cmd Msg )
openNewForm model =
    ( { model | noteForm = { emptyNoteForm | open = True, owner = noteTabOwner model.noteTab } }, Cmd.none )



-- | Populate the form with an existing note's data


openEditForm : CoupleNote -> Model -> ( Model, Cmd Msg )
openEditForm note model =
    ( { model
        | noteForm =
            { open = True
            , editId = Just note.id
            , owner = note.owner
            , title = note.title
            , body = note.body
            , showValidation = False
            }
      }
    , Cmd.none
    )



-- | Close and reset the form


closeForm : Model -> ( Model, Cmd Msg )
closeForm model =
    ( { model | noteForm = emptyNoteForm }, Cmd.none )



-- | Update the owner dropdown


setOwner : String -> Model -> ( Model, Cmd Msg )
setOwner v model =
    ( { model | noteForm = setNoteField (\f -> { f | owner = v }) model.noteForm }, Cmd.none )



-- | Update the title field


setTitle : String -> Model -> ( Model, Cmd Msg )
setTitle v model =
    ( { model | noteForm = setNoteField (\f -> { f | title = v }) model.noteForm }, Cmd.none )



-- | Update the body field


setBody : String -> Model -> ( Model, Cmd Msg )
setBody v model =
    ( { model | noteForm = setNoteField (\f -> { f | body = v }) model.noteForm }, Cmd.none )



-- | Switch tabs; also updates the owner in a new (not editing) form to match the new tab


setTab : NoteTab -> Model -> ( Model, Cmd Msg )
setTab tab model =
    let
        updatedForm =
            if model.noteForm.open && model.noteForm.editId == Nothing then
                setNoteField (\f -> { f | owner = noteTabOwner tab }) model.noteForm

            else
                model.noteForm
    in
    ( { model | noteTab = tab, noteForm = updatedForm }, Cmd.none )



-- | Update the search filter string


setSearch : String -> Model -> ( Model, Cmd Msg )
setSearch v model =
    ( { model | noteSearch = v }, Cmd.none )



-- | Validate required fields, check for duplicate title, then create or update


submitForm : Model -> ( Model, Cmd Msg )
submitForm model =
    if noteFormValid model.noteForm then
        if noteTitleExists model model.noteForm then
            ( { model | infoDialog = Just "A note with this title already exists. Please choose another title to keep the notebook tidy." }, Cmd.none )

        else
            case model.noteForm.editId of
                Nothing ->
                    ( model, createNoteReq model.noteForm )

                Just id ->
                    ( model, updateNoteReq id model.noteForm )

    else
        ( { model | noteForm = setNoteField (\f -> { f | showValidation = True }) model.noteForm }, Cmd.none )



-- | Send a DELETE request for the given note id


deleteNote : Int -> Model -> ( Model, Cmd Msg )
deleteNote id model =
    ( model, deleteNoteReq id )
