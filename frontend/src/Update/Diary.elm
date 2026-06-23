module Update.Diary exposing (..)

{-| Update handlers for the personal diary feature
-}

import Api.Requests exposing (createDiaryReq, deleteDiaryReq, refreshDiary, updateDiaryReq)
import Utils.Helpers exposing (httpErr, mapStatus, mutationFailure)
import Http
import Init exposing (emptyDiaryForm)
import Types exposing (..)


-- | Format today as YYYY-MM-DD for date inputs and ISO string comparison
todayString : { year : Int, month : Int, day : Int } -> String
todayString d =
    String.fromInt d.year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt d.month)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt d.day)


-- | Empty form pre-filled with today's date and the current tab's owner, used after save or cancel
freshForm : Model -> DiaryForm
freshForm model =
    let
        owner =
            case model.diaryTab of
                LeftDiary ->
                    "left"

                RightDiary ->
                    "right"
    in
    { emptyDiaryForm | date = todayString model.today, owner = owner, open = True }


-- | Store the diary entry list from GET /api/diary, or record the error
gotDiary : Result Http.Error (List DiaryEntry) -> Model -> ( Model, Cmd Msg )
gotDiary result model =
    case result of
        Ok entries ->
            ( { model | diaryEntries = Loaded entries }, Cmd.none )

        Err e ->
            ( { model | diaryEntries = Failed (httpErr e) }, Cmd.none )


-- | On success: reset the form and reload, on failure: show an error dialog
diaryEntryCreated : Result Http.Error DiaryEntry -> Model -> ( Model, Cmd Msg )
diaryEntryCreated result model =
    case result of
        Ok _ ->
            ( { model | diaryForm = freshForm model }, refreshDiary )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Same as diaryEntryCreated but for PUT, reset the form and reload
diaryEntryUpdated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
diaryEntryUpdated result model =
    case result of
        Ok _ ->
            ( { model | diaryForm = freshForm model }, refreshDiary )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Remove the deleted entry from the local list immediately without a full reload
diaryEntryDeleted : Int -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
diaryEntryDeleted id result model =
    case result of
        Ok _ ->
            let
                updated =
                    mapStatus (List.filter (\e -> e.id /= id)) model.diaryEntries
            in
            ( { model | diaryEntries = updated }, Cmd.none )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Reset the form to a fresh state for a new entry
openNewDiaryEntry : Model -> ( Model, Cmd Msg )
openNewDiaryEntry model =
    ( { model | diaryForm = freshForm model }, Cmd.none )


-- | Populate the form with an existing entry's data, trims timestamp to YYYY-MM-DD for the date input
openEditDiaryEntry : DiaryEntry -> Model -> ( Model, Cmd Msg )
openEditDiaryEntry entry model =
    ( { model
        | diaryForm =
            { open = True
            , editId = Just entry.id
            , owner = entry.owner
            , date = String.left 10 entry.date
            , body = entry.body
            , showValidation = False
            }
      }
    , Cmd.none
    )


-- | Cancel editing, returns the form to new-entry mode rather than hiding it
closeDiaryForm : Model -> ( Model, Cmd Msg )
closeDiaryForm model =
    ( { model | diaryForm = freshForm model }, Cmd.none )


-- | Update the date field while the user types
setDiaryDate : String -> Model -> ( Model, Cmd Msg )
setDiaryDate v model =
    let
        form =
            model.diaryForm
    in
    ( { model | diaryForm = { form | date = v } }, Cmd.none )


-- | Update the body textarea while the user types
setDiaryBody : String -> Model -> ( Model, Cmd Msg )
setDiaryBody v model =
    let
        form =
            model.diaryForm
    in
    ( { model | diaryForm = { form | body = v } }, Cmd.none )


-- | Switch tabs and reset the form so an in-progress edit is not saved under the wrong owner
setDiaryTab : DiaryTab -> Model -> ( Model, Cmd Msg )
setDiaryTab tab model =
    ( { model | diaryTab = tab, diaryForm = freshForm { model | diaryTab = tab } }, Cmd.none )


-- | Validate (non-empty, not future date), then send a create or update request
submitDiaryForm : Model -> ( Model, Cmd Msg )
submitDiaryForm model =
    let
        form =
            model.diaryForm

        today =
            todayString model.today
    in
    if String.isEmpty (String.trim form.date) || String.isEmpty (String.trim form.body) || form.date > today then
        ( { model | diaryForm = { form | showValidation = True } }, Cmd.none )

    else
        case form.editId of
            Nothing ->
                ( model, createDiaryReq form )

            Just id ->
                ( model, updateDiaryReq id form )


-- | Send a DELETE request for the given entry id
deleteDiaryEntry : Int -> Model -> ( Model, Cmd Msg )
deleteDiaryEntry id model =
    ( model, deleteDiaryReq id )
