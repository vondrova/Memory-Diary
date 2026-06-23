module Update.Memories exposing (..)

{-| Update handlers for memories: CRUD, photos, tags, and geocoding
-}

import Api.Requests
    exposing
        ( createMemoryReq
        , deleteMemoryReq
        , fetchAddressSuggestions
        , refreshMemories
        , updateMemoryReq
        , uploadPhotos
        )
import File exposing (File)
import File.Select as Select
import Utils.FormUtils exposing (memFormValid, setField)
import Utils.Helpers exposing (httpErr, isSafePhotoName, mapStatus, mutationFailure, photoAcceptList)
import Http
import Init exposing (emptyMemForm)
import Process
import Task
import Types exposing (..)


-- | Store all memories; used by home-page panels and the calendar
gotMemories : Result Http.Error (List Memory) -> Model -> ( Model, Cmd Msg )
gotMemories result model =
    case result of
        Ok ms ->
            ( { model | memories = Loaded ms }, Cmd.none )

        Err e ->
            ( { model | memories = Failed (httpErr e) }, Cmd.none )


-- | Store the filtered timeline view (separate from the full list used by panels)
gotTimelineMemories : Result Http.Error (List Memory) -> Model -> ( Model, Cmd Msg )
gotTimelineMemories result model =
    case result of
        Ok ms ->
            ( { model | timelineMemories = Loaded ms }, Cmd.none )

        Err e ->
            ( { model | timelineMemories = Failed (httpErr e) }, Cmd.none )


-- | Store memories matching today's date from past years
gotOnThisDay : Result Http.Error (List Memory) -> Model -> ( Model, Cmd Msg )
gotOnThisDay result model =
    case result of
        Ok ms ->
            ( { model | onThisDay = Loaded ms }, Cmd.none )

        Err _ ->
            ( { model | onThisDay = Loaded [] }, Cmd.none )


-- | Store aggregated stats from /api/stats, or record the error
gotStats : Result Http.Error Stats -> Model -> ( Model, Cmd Msg )
gotStats result model =
    case result of
        Ok s ->
            ( { model | stats = Loaded s }, Cmd.none )

        Err e ->
            ( { model | stats = Failed (httpErr e) }, Cmd.none )


-- | On success: reset the form and reload with the current filter
memoryCreated : Result Http.Error Memory -> Model -> ( Model, Cmd Msg )
memoryCreated result model =
    case result of
        Ok _ ->
            ( { model | memForm = emptyMemForm, tagInput = "", photoInput = "" }, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Same as memoryCreated but for PUT
memoryUpdated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
memoryUpdated result model =
    case result of
        Ok _ ->
            ( { model | memForm = emptyMemForm, tagInput = "", photoInput = "" }, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Optimistically remove the memory from the local list, then reload
memoryDeleted : Int -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
memoryDeleted id result model =
    case result of
        Ok _ ->
            let
                updated =
                    mapStatus (List.filter (\m -> m.id /= id)) model.memories
            in
            ( { model | memories = updated }, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Open a blank memory form
openNewForm : Model -> ( Model, Cmd Msg )
openNewForm model =
    ( { model | memForm = { emptyMemForm | open = True }, tagInput = "", photoInput = "" }, Cmd.none )


-- | Populate the form with an existing memory's fields, trims timestamps to 16 chars for datetime-local inputs
openEditForm : Memory -> Model -> ( Model, Cmd Msg )
openEditForm mem model =
    ( { model
        | memForm =
            { open = True
            , editId = Just mem.id
            , title = mem.title
            , timeFrom = String.left 16 mem.timeFrom
            , timeTo = String.left 16 mem.timeTo
            , description = Maybe.withDefault "" mem.description
            , location = Maybe.withDefault "" mem.location
            , locSuggestOpen = False
            , tagSuggestOpen = False
            , tags = mem.tags
            , photos = mem.photos
            , showValidation = False
            }
        , tagInput = ""
        , photoInput = ""
      }
    , Cmd.none
    )


-- | Close and reset the form
closeForm : Model -> ( Model, Cmd Msg )
closeForm model =
    ( { model | memForm = emptyMemForm, tagInput = "", photoInput = "" }, Cmd.none )


-- | Update the title field
setTitle : String -> Model -> ( Model, Cmd Msg )
setTitle v model =
    ( { model | memForm = setField (\f -> { f | title = v }) model.memForm }, Cmd.none )


-- | Update the start datetime field
setTimeFrom : String -> Model -> ( Model, Cmd Msg )
setTimeFrom v model =
    ( { model | memForm = setField (\f -> { f | timeFrom = v }) model.memForm }, Cmd.none )


-- | Update the end datetime field
setTimeTo : String -> Model -> ( Model, Cmd Msg )
setTimeTo v model =
    ( { model | memForm = setField (\f -> { f | timeTo = v }) model.memForm }, Cmd.none )


-- | Update the description field
setDescription : String -> Model -> ( Model, Cmd Msg )
setDescription v model =
    ( { model | memForm = setField (\f -> { f | description = v }) model.memForm }, Cmd.none )


-- | Update the location field and schedule a Nominatim geocode after a 300 ms debounce
setLocation : String -> Model -> ( Model, Cmd Msg )
setLocation v model =
    let
        trimmed =
            String.trim v

        newSeq =
            model.geocodeSeq + 1
    in
    ( { model
        | memForm = setField (\f -> { f | location = v, locSuggestOpen = False }) model.memForm
        , addressSuggestions =
            if String.length trimmed < 2 then
                []

            else
                model.addressSuggestions
        , geocodeSeq = newSeq
      }
    , if String.length trimmed < 2 then
        Cmd.none

      else
        Task.perform (\_ -> DoGeocodeIfCurrent newSeq trimmed) (Process.sleep 300)
    )


-- | Fire the geocode request only if the sequence number still matches (suppresses stale responses)
doGeocodeIfCurrent : Int -> String -> Model -> ( Model, Cmd Msg )
doGeocodeIfCurrent seq query model =
    if seq /= model.geocodeSeq then
        ( model, Cmd.none )

    else
        ( model, fetchAddressSuggestions query )


-- | Store geocoding results; discards them if the location field has changed since the request was sent
gotAddressSuggestions : String -> Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
gotAddressSuggestions query result model =
    let
        current =
            String.trim model.memForm.location
    in
    if not (String.startsWith query current) then
        ( model, Cmd.none )

    else
        case result of
            Ok suggestions ->
                ( { model | addressSuggestions = suggestions }, Cmd.none )

            Err _ ->
                ( { model | addressSuggestions = [] }, Cmd.none )


-- | Update the tag text input
setTagInput : String -> Model -> ( Model, Cmd Msg )
setTagInput v model =
    ( { model | tagInput = v }, Cmd.none )


-- | Add the trimmed tag to the form's tag list; no-op for empty input or duplicates
addTag : Model -> ( Model, Cmd Msg )
addTag model =
    let
        trimmed =
            String.trim model.tagInput
    in
    if String.isEmpty trimmed || List.member trimmed model.memForm.tags then
        ( { model | tagInput = "" }, Cmd.none )

    else
        ( { model | memForm = setField (\f -> { f | tags = f.tags ++ [ trimmed ] }) model.memForm, tagInput = "" }, Cmd.none )


-- | Remove a tag from the form's tag list
removeTag : String -> Model -> ( Model, Cmd Msg )
removeTag t model =
    ( { model | memForm = setField (\f -> { f | tags = List.filter (\x -> x /= t) f.tags }) model.memForm }, Cmd.none )


-- | Update the photo filename input
setPhotoInput : String -> Model -> ( Model, Cmd Msg )
setPhotoInput v model =
    ( { model | photoInput = v }, Cmd.none )


-- | Append the photo filename to the list, validates the name format and rejects duplicates
addPhotoByName : Model -> ( Model, Cmd Msg )
addPhotoByName model =
    let
        trimmed =
            String.trim model.photoInput
    in
    if not (isSafePhotoName trimmed) || List.member trimmed model.memForm.photos then
        ( { model | photoInput = "" }, Cmd.none )

    else
        ( { model
            | memForm = setField (\f -> { f | photos = f.photos ++ [ trimmed ] }) model.memForm
            , photoInput = ""
          }
        , Cmd.none
        )


-- | Validate required fields, then send a create or update request
submitMemForm : Model -> ( Model, Cmd Msg )
submitMemForm model =
    if memFormValid model.memForm then
        case model.memForm.editId of
            Nothing ->
                ( model, createMemoryReq model.memForm )

            Just id ->
                ( model, updateMemoryReq id model.memForm )

    else
        ( { model | memForm = setField (\f -> { f | showValidation = True }) model.memForm }, Cmd.none )


-- | Send a DELETE request for the given memory id
deleteMemory : Int -> Model -> ( Model, Cmd Msg )
deleteMemory id model =
    ( model, deleteMemoryReq id )


-- | Open the native file picker for multi-file photo selection
pickPhotos : Model -> ( Model, Cmd Msg )
pickPhotos model =
    ( model, Select.files photoAcceptList FilesSelected )


-- | Upload the selected files via multipart POST to /api/photos
filesSelected : File -> List File -> Model -> ( Model, Cmd Msg )
filesSelected first rest model =
    ( model, uploadPhotos (first :: rest) )


-- | Append the returned filenames to the form's photo list
photosUploaded : Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
photosUploaded result model =
    case result of
        Ok names ->
            ( { model | memForm = setField (\f -> { f | photos = f.photos ++ names }) model.memForm }, Cmd.none )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Remove a photo filename from the form's photo list
removePhoto : String -> Model -> ( Model, Cmd Msg )
removePhoto fn model =
    ( { model | memForm = setField (\f -> { f | photos = List.filter (\p -> p /= fn) f.photos }) model.memForm }
    , Cmd.none
    )


-- | Add a tag selected from the dropdown, close the dropdown
addTagFromCombo : String -> Model -> ( Model, Cmd Msg )
addTagFromCombo tag model =
    let
        trimmed =
            String.trim tag
    in
    if String.isEmpty trimmed || List.member trimmed model.memForm.tags then
        ( model, Cmd.none )

    else
        ( { model | memForm = setField (\f -> { f | tags = f.tags ++ [ trimmed ], tagSuggestOpen = False }) model.memForm, tagInput = "" }
        , Cmd.none
        )


-- | Toggle the location suggestion dropdown, close the tag dropdown
toggleLocSuggest : Model -> ( Model, Cmd Msg )
toggleLocSuggest model =
    ( { model | memForm = setField (\f -> { f | locSuggestOpen = not f.locSuggestOpen, tagSuggestOpen = False }) model.memForm }, Cmd.none )


-- | Fill the location field with the chosen suggestion and clear the suggestion list
selectLocSuggest : String -> Model -> ( Model, Cmd Msg )
selectLocSuggest loc model =
    ( { model
        | memForm = setField (\f -> { f | location = loc, locSuggestOpen = False }) model.memForm
        , addressSuggestions = []
      }
    , Cmd.none
    )


-- | Toggle the tag suggestion dropdown, close the location dropdown
toggleTagSuggest : Model -> ( Model, Cmd Msg )
toggleTagSuggest model =
    ( { model | memForm = setField (\f -> { f | tagSuggestOpen = not f.tagSuggestOpen, locSuggestOpen = False }) model.memForm }, Cmd.none )
