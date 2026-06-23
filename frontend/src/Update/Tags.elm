module Update.Tags exposing (..)

{-| Update handlers for bulk tag and place management
-}

import Api.Requests
    exposing
        ( createLocationReq
        , createTagReq
        , deleteLocationReq
        , deleteTagReq
        , fetchLocationCatalog
        , fetchTagCatalog
        , refreshMemories
        , renameLocationReq
        , renameTagReq
        )
import Http
import Types exposing (..)
import Utils.Helpers exposing (mutationFailure)



-- | Store the tag catalog


gotTagCatalog : Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
gotTagCatalog result model =
    case result of
        Ok tags ->
            ( { model | tagCatalog = tags }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )



-- | Store the location catalog


gotLocationCatalog : Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
gotLocationCatalog result model =
    case result of
        Ok locations ->
            ( { model | locationCatalog = locations }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )



-- | Open the tag management panel


openTagPanel : Model -> ( Model, Cmd Msg )
openTagPanel model =
    ( { model | tagForm = { open = True, editOldName = Nothing, editNewName = "", newName = "" } }, Cmd.none )



-- | Close the tag management panel


closeTagPanel : Model -> ( Model, Cmd Msg )
closeTagPanel model =
    ( { model | tagForm = { open = False, editOldName = Nothing, editNewName = "", newName = "" } }, Cmd.none )



-- | Open the inline rename input for a specific tag


startEditTag : String -> Model -> ( Model, Cmd Msg )
startEditTag name model =
    ( { model | tagForm = { open = True, editOldName = Just name, editNewName = name, newName = model.tagForm.newName } }, Cmd.none )



-- | Cancel the inline tag rename without saving


cancelEditTag : Model -> ( Model, Cmd Msg )
cancelEditTag model =
    let
        tf =
            model.tagForm
    in
    ( { model | tagForm = { tf | editOldName = Nothing, editNewName = "" } }, Cmd.none )



-- | Update the rename input for the tag being edited


setTagEditName : String -> Model -> ( Model, Cmd Msg )
setTagEditName s model =
    let
        tf =
            model.tagForm
    in
    ( { model | tagForm = { tf | editNewName = s } }, Cmd.none )



-- | Save the tag rename (no-op if the new name is empty or unchanged)


submitTagRename : Model -> ( Model, Cmd Msg )
submitTagRename model =
    case model.tagForm.editOldName of
        Nothing ->
            ( model, Cmd.none )

        Just oldName ->
            let
                tf =
                    model.tagForm

                newName =
                    String.trim tf.editNewName

                newTagForm =
                    { tf | editOldName = Nothing, editNewName = "" }
            in
            if String.isEmpty newName || newName == oldName then
                ( { model | tagForm = newTagForm }, Cmd.none )

            else
                ( { model | tagForm = newTagForm }
                , renameTagReq oldName newName
                )



-- | Reload memories after a tag rename (tag name is stored per memory)


tagRenamed : Result Http.Error () -> Model -> ( Model, Cmd Msg )
tagRenamed result model =
    case result of
        Ok _ ->
            ( model, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Send a DELETE request to remove a tag from all memories


deleteTagFromAll : String -> Model -> ( Model, Cmd Msg )
deleteTagFromAll tag model =
    ( model, deleteTagReq tag )



-- | Reload memories after a tag delete.


tagDeleted : Result Http.Error () -> Model -> ( Model, Cmd Msg )
tagDeleted result model =
    case result of
        Ok _ ->
            ( model, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Update the new-tag name input


setTagNewName : String -> Model -> ( Model, Cmd Msg )
setTagNewName s model =
    let
        tf =
            model.tagForm
    in
    ( { model | tagForm = { tf | newName = s } }, Cmd.none )



-- | Create a new tag (no-op for empty names or names already in the catalog)


submitNewTag : Model -> ( Model, Cmd Msg )
submitNewTag model =
    let
        name =
            String.trim model.tagForm.newName

        tf =
            model.tagForm
    in
    if String.isEmpty name || List.member name model.tagCatalog then
        ( model, Cmd.none )

    else
        ( { model | tagForm = { tf | newName = "" } }, createTagReq name )



-- | Reload the tag catalog after a successful create


tagCreated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
tagCreated result model =
    case result of
        Ok _ ->
            ( model, fetchTagCatalog )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Open the location management panel


openLocationPanel : Model -> ( Model, Cmd Msg )
openLocationPanel model =
    ( { model | locationForm = { open = True, editOldName = Nothing, editNewName = "", newName = "" } }, Cmd.none )



-- | Close the location management panel


closeLocationPanel : Model -> ( Model, Cmd Msg )
closeLocationPanel model =
    ( { model | locationForm = { open = False, editOldName = Nothing, editNewName = "", newName = "" } }, Cmd.none )



-- | Open the inline rename input for a specific location


startEditLocation : String -> Model -> ( Model, Cmd Msg )
startEditLocation name model =
    ( { model | locationForm = { open = True, editOldName = Just name, editNewName = name, newName = model.locationForm.newName } }, Cmd.none )



-- | Cancel the inline location rename without saving


cancelEditLocation : Model -> ( Model, Cmd Msg )
cancelEditLocation model =
    let
        lf =
            model.locationForm
    in
    ( { model | locationForm = { lf | editOldName = Nothing, editNewName = "" } }, Cmd.none )



-- | Update the rename input for the location being edited


setLocationEditName : String -> Model -> ( Model, Cmd Msg )
setLocationEditName s model =
    let
        lf =
            model.locationForm
    in
    ( { model | locationForm = { lf | editNewName = s } }, Cmd.none )



-- | Save the location rename (no-op if the new name is empty or unchanged)


submitLocationRename : Model -> ( Model, Cmd Msg )
submitLocationRename model =
    case model.locationForm.editOldName of
        Nothing ->
            ( model, Cmd.none )

        Just oldName ->
            let
                lf =
                    model.locationForm

                newName =
                    String.trim lf.editNewName

                newLocForm =
                    { lf | editOldName = Nothing, editNewName = "" }
            in
            if String.isEmpty newName || newName == oldName then
                ( { model | locationForm = newLocForm }, Cmd.none )

            else
                ( { model | locationForm = newLocForm }
                , renameLocationReq oldName newName
                )



-- | Reload memories after a location rename (location is stored per memory)


locationRenamed : Result Http.Error () -> Model -> ( Model, Cmd Msg )
locationRenamed result model =
    case result of
        Ok _ ->
            ( model, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Send a DELETE request to remove a location from all memories


deleteLocationFromAll : String -> Model -> ( Model, Cmd Msg )
deleteLocationFromAll loc model =
    ( model, deleteLocationReq loc )



-- | Reload memories after a location delete


locationDeleted : Result Http.Error () -> Model -> ( Model, Cmd Msg )
locationDeleted result model =
    case result of
        Ok _ ->
            ( model, refreshMemories model.filter )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )



-- | Update the new-location name input


setLocationNewName : String -> Model -> ( Model, Cmd Msg )
setLocationNewName s model =
    let
        lf =
            model.locationForm
    in
    ( { model | locationForm = { lf | newName = s } }, Cmd.none )



-- | Create a new location (no-op for empty names or names already in the catalog)


submitNewLocation : Model -> ( Model, Cmd Msg )
submitNewLocation model =
    let
        name =
            String.trim model.locationForm.newName

        lf =
            model.locationForm
    in
    if String.isEmpty name || List.member name model.locationCatalog then
        ( model, Cmd.none )

    else
        ( { model | locationForm = { lf | newName = "" } }, createLocationReq name )



-- | Reload the location catalog after a successful create


locationCreated : Result Http.Error () -> Model -> ( Model, Cmd Msg )
locationCreated result model =
    case result of
        Ok _ ->
            ( model, fetchLocationCatalog )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )
