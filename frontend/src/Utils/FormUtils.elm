module Utils.FormUtils exposing (..)

{-| Shared form helpers used across update submodules
-}

import Types exposing (..)
import Utils.Helpers exposing (isBlank)



-- | Apply a function to a MemForm (named wrapper for clarity at call sites)


setField : (MemForm -> MemForm) -> MemForm -> MemForm
setField f form =
    f form



-- | Apply a function to an ImportantDayForm


setImportantDayField : (ImportantDayForm -> ImportantDayForm) -> ImportantDayForm -> ImportantDayForm
setImportantDayField f form =
    f form



-- | Apply a function to a NoteForm


setNoteField : (NoteForm -> NoteForm) -> NoteForm -> NoteForm
setNoteField f form =
    f form



-- | Apply a function to a PlanForm


setPlanField : (PlanForm -> PlanForm) -> PlanForm -> PlanForm
setPlanField f form =
    f form



-- | True when required fields are filled and timeFrom <= timeTo


memFormValid : MemForm -> Bool
memFormValid form =
    memRequiredFieldsValid form && validTimeOrder form



-- | True when title, timeFrom, and timeTo are all non-blank


memRequiredFieldsValid : MemForm -> Bool
memRequiredFieldsValid form =
    not (isBlank form.title) && not (isBlank form.timeFrom) && not (isBlank form.timeTo)



-- | True when the start time is not after the end time, blank fields are treated as valid


validTimeOrder : MemForm -> Bool
validTimeOrder form =
    isBlank form.timeFrom || isBlank form.timeTo || form.timeFrom <= form.timeTo



-- | True when another note with the same owner and title already exists
-- The backend enforces the same uniqueness rule, this is a client-side UX guard


noteTitleExists : Model -> NoteForm -> Bool
noteTitleExists model form =
    let
        normalizedTitle =
            String.toLower (String.trim form.title)

        sameRecord note =
            case form.editId of
                Just id ->
                    note.id == id

                Nothing ->
                    False
    in
    case model.notes of
        Loaded notes ->
            notes
                |> List.any
                    (\note ->
                        not (sameRecord note)
                            && note.owner
                            == form.owner
                            && String.toLower (String.trim note.title)
                            == normalizedTitle
                    )

        _ ->
            False



-- | Convert a CouplePlan to a PlanForm ready for editing


planToForm : Bool -> CouplePlan -> PlanForm
planToForm open plan =
    { open = open
    , editId = Just plan.id
    , category = plan.category
    , catSuggestOpen = False
    , title = plan.title
    , detail = Maybe.withDefault "" plan.detail
    , done = plan.done
    , showValidation = False
    }



-- | Apply a function to whichever AvatarProfile corresponds to the given side


updateAvatar : AvatarSide -> (AvatarProfile -> AvatarProfile) -> Model -> Model
updateAvatar side f model =
    case side of
        LeftAvatar ->
            { model | leftAvatar = f model.leftAvatar }

        RightAvatar ->
            { model | rightAvatar = f model.rightAvatar }



-- | Convert a NoteTab to the owner string stored in the database ("shared", "left", "right")


noteTabOwner : NoteTab -> String
noteTabOwner tab =
    case tab of
        SharedNotes ->
            "shared"

        LeftAvatarNotes ->
            "left"

        RightAvatarNotes ->
            "right"
