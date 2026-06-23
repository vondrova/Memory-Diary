module Update.ImportantDays exposing
    ( closeForm
    , deleteImportantDay
    , gotImportantDays
    , importantDayCreated
    , importantDayDeleted
    , importantDayUpdated
    , openEditForm
    , openNewForm
    , setDate
    , setNote
    , setTitle
    , submitForm
    )

{-| Update handlers for important days
-}

import Api.Requests
    exposing
        ( createImportantDayReq
        , deleteImportantDayReq
        , refreshImportantDays
        , updateImportantDayReq
        )
import Http
import Init exposing (emptyImportantDayForm)
import Types exposing (..)
import Utils.FormUtils exposing (setImportantDayField)
import Utils.Helpers exposing (httpErr, importantDayFormValid, mapStatus, mutationFailure)



-- | Store the important-days list from the API, or record the error


gotImportantDays : Result Http.Error (List ImportantDay) -> Model -> ( Model, Cmd Msg )
gotImportantDays result model =
    case result of
        Ok as_ ->
            ( { model | importantDays = Loaded as_ }, Cmd.none )

        Err e ->
            ( { model | importantDays = Failed (httpErr e) }, Cmd.none )



-- | On success: close the form and reload, on failure: show error dialog


importantDayCreated : Result Http.Error ImportantDay -> Model -> ( Model, Cmd Msg )
importantDayCreated result model =
    case result of
        Ok _ ->
            ( { model | importantDayForm = emptyImportantDayForm }, refreshImportantDays )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Same as importantDayCreated but for PUT


importantDayUpdated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
importantDayUpdated result model =
    case result of
        Ok _ ->
            ( { model | importantDayForm = emptyImportantDayForm }, refreshImportantDays )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Optimistically remove the day from the local list, then reload


importantDayDeleted : Int -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
importantDayDeleted id result model =
    case result of
        Ok _ ->
            let
                updated =
                    mapStatus (List.filter (\a -> a.id /= Just id)) model.importantDays
            in
            ( { model | importantDays = updated }, refreshImportantDays )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Open a blank form for creating a new important day


openNewForm : Model -> ( Model, Cmd Msg )
openNewForm model =
    ( { model | importantDayForm = { emptyImportantDayForm | open = True } }, Cmd.none )



-- | Populate the form with an existing day's data (no-op for read-only days without an id)


openEditForm : ImportantDay -> Model -> ( Model, Cmd Msg )
openEditForm ann model =
    case ann.id of
        Nothing ->
            ( model, Cmd.none )

        Just id ->
            ( { model
                | importantDayForm =
                    { open = True
                    , editId = Just id
                    , title = ann.title
                    , date = String.left 10 ann.date
                    , note = Maybe.withDefault "" ann.note
                    , showValidation = False
                    }
              }
            , Cmd.none
            )



-- | Close and reset the form


closeForm : Model -> ( Model, Cmd Msg )
closeForm model =
    ( { model | importantDayForm = emptyImportantDayForm }, Cmd.none )



-- | Update the title field


setTitle : String -> Model -> ( Model, Cmd Msg )
setTitle v model =
    ( { model | importantDayForm = setImportantDayField (\f -> { f | title = v }) model.importantDayForm }, Cmd.none )



-- | Update the date field


setDate : String -> Model -> ( Model, Cmd Msg )
setDate v model =
    ( { model | importantDayForm = setImportantDayField (\f -> { f | date = v }) model.importantDayForm }, Cmd.none )



-- | Update the note field


setNote : String -> Model -> ( Model, Cmd Msg )
setNote v model =
    ( { model | importantDayForm = setImportantDayField (\f -> { f | note = v }) model.importantDayForm }, Cmd.none )



-- | Validate required fields, then create or update


submitForm : Model -> ( Model, Cmd Msg )
submitForm model =
    if importantDayFormValid model.importantDayForm then
        case model.importantDayForm.editId of
            Nothing ->
                ( model, createImportantDayReq model.importantDayForm )

            Just id ->
                ( model, updateImportantDayReq id model.importantDayForm )

    else
        ( { model | importantDayForm = setImportantDayField (\f -> { f | showValidation = True }) model.importantDayForm }, Cmd.none )



-- | Send a DELETE request for the given day id


deleteImportantDay : Int -> Model -> ( Model, Cmd Msg )
deleteImportantDay id model =
    ( model, deleteImportantDayReq id )
