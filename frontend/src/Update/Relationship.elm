module Update.Relationship exposing (..)

{-| Update handlers for relationship settings and partner avatar profiles
-}

import Api.Requests exposing (fetchImportantDays, saveProfile, saveRelationship, uploadAvatarPhoto)
import File exposing (File)
import File.Select as Select
import Utils.Helpers exposing (avatarBySide, httpErr, mutationFailure, photoAcceptList, profileDataToAvatar, relationshipStartDate)
import Http
import Types exposing (..)


-- | Store relationship data and sync related model fields (date, heart color)
gotRelationship : Result Http.Error RelationshipData -> Model -> ( Model, Cmd Msg )
gotRelationship result model =
    case result of
        Ok rel ->
            ( { model
                | relationship = Loaded rel
                , relationshipDate = Maybe.withDefault model.relationshipDate (Maybe.map (String.left 10) rel.startDate)
                , heartColor = rel.heartColor
              }
            , Cmd.none
            )

        Err e ->
            ( { model | relationship = Failed (httpErr e) }, Cmd.none )


-- | Store the left partner's profile and convert it to the local AvatarProfile format
gotLeftProfile : Result Http.Error ProfileData -> Model -> ( Model, Cmd Msg )
gotLeftProfile result model =
    case result of
        Ok p ->
            ( { model | leftProfile = Loaded p, leftAvatar = profileDataToAvatar p }, Cmd.none )

        Err e ->
            ( { model | leftProfile = Failed (httpErr e) }, Cmd.none )


-- | Store the right partner's profile and convert it to the local AvatarProfile format
gotRightProfile : Result Http.Error ProfileData -> Model -> ( Model, Cmd Msg )
gotRightProfile result model =
    case result of
        Ok p ->
            ( { model | rightProfile = Loaded p, rightAvatar = profileDataToAvatar p }, Cmd.none )

        Err e ->
            ( { model | rightProfile = Failed (httpErr e) }, Cmd.none )


-- | On success: update model fields from the saved data and close all editors
relationshipSaved : Result Http.Error RelationshipData -> Model -> ( Model, Cmd Msg )
relationshipSaved result model =
    case result of
        Ok rel ->
            ( { model
                | relationship = Loaded rel
                , heartColor = rel.heartColor
                , relationshipDate = Maybe.withDefault model.relationshipDate (Maybe.map (String.left 10) rel.startDate)
                , editingHeart = False
                , editingRelationship = False
                , relationshipValidation = False
              }
            , Cmd.none
            )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | On success: update the side's avatar and reload important days (birthday may have changed)
profileSaved : String -> Result Http.Error ProfileData -> Model -> ( Model, Cmd Msg )
profileSaved side result model =
    case result of
        Ok p ->
            let
                updated =
                    if side == "left" then
                        { model | leftProfile = Loaded p, leftAvatar = profileDataToAvatar p, editingAvatar = Nothing }

                    else
                        { model | rightProfile = Loaded p, rightAvatar = profileDataToAvatar p, editingAvatar = Nothing }
            in
            ( updated, fetchImportantDays )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Update the relationship start date input
setDate : String -> Model -> ( Model, Cmd Msg )
setDate v model =
    ( { model | relationshipDate = v, relationshipValidation = False }, Cmd.none )


-- | Update the heart color selection
setHeartColor : String -> Model -> ( Model, Cmd Msg )
setHeartColor v model =
    ( { model | heartColor = v }, Cmd.none )


-- | Open the heart editor; pre-fills the date from the stored relationship when available
openHeartEditor : Model -> ( Model, Cmd Msg )
openHeartEditor model =
    let
        storedDate =
            let
                fromRelationship =
                    relationshipStartDate model.relationship
            in
            if String.isEmpty fromRelationship then
                model.relationshipDate

            else
                fromRelationship
    in
    ( { model | editingHeart = True, relationshipDate = storedDate }, Cmd.none )


-- | Close the heart editor and restore the stored date
closeHeartEditor : Model -> ( Model, Cmd Msg )
closeHeartEditor model =
    let
        storedDate =
            let
                fromRelationship =
                    relationshipStartDate model.relationship
            in
            if String.isEmpty fromRelationship then
                model.relationshipDate

            else
                fromRelationship
    in
    ( { model | editingHeart = False, relationshipDate = storedDate }, Cmd.none )


-- | Send the relationship start date and heart color to the API
submitRelationship : Model -> ( Model, Cmd Msg )
submitRelationship model =
    ( model, saveRelationship model.relationshipDate model.heartColor )


-- | Update the name field in the draft avatar
setAvatarName : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarName _ v model =
    ( { model | draftAvatar = Maybe.map (\a -> { a | name = v }) model.draftAvatar }, Cmd.none )


-- | Update the display mode (figure/photo) in the draft avatar
setAvatarMode : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarMode _ v model =
    ( { model | draftAvatar = Maybe.map (\a -> { a | mode = v }) model.draftAvatar }, Cmd.none )


-- | Update the photo filename in the draft avatar; also switches to photo mode when a name is set
setAvatarPhoto : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarPhoto _ v model =
    ( { model
        | draftAvatar =
            Maybe.map
                (\a ->
                    { a
                        | photo = v
                        , mode =
                            if String.isEmpty (String.trim v) then
                                a.mode

                            else
                                "photo"
                    }
                )
                model.draftAvatar
      }
    , Cmd.none
    )


-- | Update the figure color in the draft avatar
setAvatarFigureColor : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarFigureColor _ v model =
    ( { model | draftAvatar = Maybe.map (\a -> { a | figureColor = v }) model.draftAvatar }, Cmd.none )


-- | Update the accessory in the draft avatar
setAvatarAccessory : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarAccessory _ v model =
    ( { model | draftAvatar = Maybe.map (\a -> { a | accessory = v }) model.draftAvatar }, Cmd.none )


-- | Update the expression in the draft avatar
setAvatarExpression : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarExpression _ v model =
    ( { model | draftAvatar = Maybe.map (\a -> { a | expression = v }) model.draftAvatar }, Cmd.none )


-- | Update the birthday in the draft avatar
setAvatarBirthday : AvatarSide -> String -> Model -> ( Model, Cmd Msg )
setAvatarBirthday _ v model =
    ( { model | draftAvatar = Maybe.map (\a -> { a | birthday = v }) model.draftAvatar }, Cmd.none )


-- | Open the native file picker for avatar photo upload
pickAvatarPhoto : AvatarSide -> Model -> ( Model, Cmd Msg )
pickAvatarPhoto side model =
    ( model, Select.files photoAcceptList (AvatarPhotoSelected side) )


-- | Upload the selected avatar photo files
avatarPhotoSelected : AvatarSide -> File -> List File -> Model -> ( Model, Cmd Msg )
avatarPhotoSelected side first rest model =
    ( model, uploadAvatarPhoto side (first :: rest) )


-- | Store the uploaded filename in the draft avatar and switch to photo mode
avatarPhotoUploaded : AvatarSide -> Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
avatarPhotoUploaded _ result model =
    case result of
        Ok (name :: _) ->
            ( { model | draftAvatar = Maybe.map (\a -> { a | photo = name, mode = "photo" }) model.draftAvatar }, Cmd.none )

        Ok [] ->
            ( { model | infoDialog = Just "No uploaded file was returned." }, Cmd.none )

        Err error ->
            ( { model | infoDialog = Just (mutationFailure error) }, Cmd.none )


-- | Open the relationship-start-date editor, pre-fills from the stored relationship
openRelationshipEditor : Model -> ( Model, Cmd Msg )
openRelationshipEditor model =
    let
        storedDate =
            let
                fromRelationship =
                    relationshipStartDate model.relationship
            in
            if String.isEmpty fromRelationship then
                model.relationshipDate

            else
                fromRelationship
    in
    ( { model | editingRelationship = True, relationshipDate = storedDate }, Cmd.none )


-- | Close the relationship editor without saving
closeRelationshipEditor : Model -> ( Model, Cmd Msg )
closeRelationshipEditor model =
    ( { model | editingRelationship = False }, Cmd.none )


-- | Open the avatar editor for the given side, copying the live avatar into the draft
openAvatarEditor : AvatarSide -> Model -> ( Model, Cmd Msg )
openAvatarEditor side model =
    ( { model | editingAvatar = Just side, draftAvatar = Just (avatarBySide side model) }, Cmd.none )


-- | Close the avatar editor and discard the draft
closeAvatarEditor : Model -> ( Model, Cmd Msg )
closeAvatarEditor model =
    ( { model | editingAvatar = Nothing, draftAvatar = Nothing }, Cmd.none )


-- | Optimistically commit the draft to the model, then save to the API
submitAvatar : AvatarSide -> Model -> ( Model, Cmd Msg )
submitAvatar side model =
    case model.draftAvatar of
        Nothing ->
            ( { model | editingAvatar = Nothing }, Cmd.none )

        Just profile ->
            let
                sideStr =
                    case side of
                        LeftAvatar ->
                            "left"

                        RightAvatar ->
                            "right"

                committed =
                    case side of
                        LeftAvatar ->
                            { model | leftAvatar = profile, editingAvatar = Nothing, draftAvatar = Nothing }

                        RightAvatar ->
                            { model | rightAvatar = profile, editingAvatar = Nothing, draftAvatar = Nothing }
            in
            ( committed, saveProfile profile sideStr )
