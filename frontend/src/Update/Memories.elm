module Update.Memories exposing (updateMemories)

import Api.Requests as Requests
import File.Select as Select
import Http
import Types exposing (..)
import Utils.FormUtils exposing (resetMemoryForm)


updateMemories : Msg -> Model -> ( Model, Cmd Msg )
updateMemories msg model =
    case msg of
        SetSearch q ->
            ( { model | searchQuery = q }, Requests.fetchMemories q GotMemories )

        OpenForm ->
            ( { model | formOpen = True }, Cmd.none )

        CloseForm ->
            ( resetMemoryForm model, Cmd.none )

        SetTitle v ->
            ( { model | formTitle = v }, Cmd.none )

        SetTimeFrom v ->
            ( { model | formTimeFrom = v }, Cmd.none )

        SetTimeTo v ->
            ( { model | formTimeTo = v }, Cmd.none )

        SetDescription v ->
            ( { model | formDescription = v }, Cmd.none )

        SetLocation v ->
            ( { model | formLocation = v, geoSuggestions = [] }, Cmd.none )

        SetTags v ->
            ( { model | formTags = v }, Cmd.none )

        PickPhoto ->
            ( model, Select.file [ "image/*" ] PhotoPicked )

        PhotoPicked file ->
            ( { model | formPendingPhoto = Just file }, Cmd.none )

        SetGeoQuery v ->
            ( { model | geoQuery = v }, Cmd.none )

        SearchGeo ->
            ( model, Requests.searchGeo model.geoQuery GotGeoSuggestions )

        GotGeoSuggestions (Ok sugg) ->
            ( { model | geoSuggestions = sugg }, Cmd.none )

        GotGeoSuggestions (Err _) ->
            ( { model | geoSuggestions = [] }, Cmd.none )

        SelectGeoSuggestion name ->
            ( { model | formLocation = name, geoSuggestions = [], geoQuery = "" }, Cmd.none )

        SubmitForm ->
            ( model
            , Requests.createMemory
                { title = model.formTitle
                , timeFrom = model.formTimeFrom
                , timeTo = model.formTimeTo
                , description = model.formDescription
                , location = model.formLocation
                , tags = model.formTags
                }
                MemoryCreated
            )

        MemoryCreated (Ok mem) ->
            ( resetMemoryForm { model | memories = model.memories ++ [ mem ] }
            , Cmd.none
            )

        MemoryCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit vzpomínku." }, Cmd.none )

        GotMemories (Ok mems) ->
            ( { model | memories = mems, error = Nothing }, Cmd.none )

        GotMemories (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst vzpomínky." }, Cmd.none )

        DeleteMemory memId ->
            let
                ( trashed, kept ) =
                    List.partition (\m -> m.id == memId) model.memories
            in
            ( { model | memories = kept, trashedMemories = model.trashedMemories ++ trashed }
            , Cmd.none
            )

        RestoreMemory memId ->
            let
                ( restored, stillTrashed ) =
                    List.partition (\m -> m.id == memId) model.trashedMemories
            in
            ( { model | trashedMemories = stillTrashed, memories = model.memories ++ restored }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
