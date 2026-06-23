module Utils.FormUtils exposing (resetMemoryForm)

import Types exposing (Model)


resetMemoryForm : Model -> Model
resetMemoryForm model =
    { model
        | formOpen = False
        , formTitle = ""
        , formTimeFrom = ""
        , formTimeTo = ""
        , formDescription = ""
        , formLocation = ""
        , formTags = ""
        , formPendingPhoto = Nothing
        , geoQuery = ""
        , geoSuggestions = []
    }
