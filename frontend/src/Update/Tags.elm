module Update.Tags exposing (updateTags)

import Types exposing (..)


updateTags : Msg -> Model -> ( Model, Cmd Msg )
updateTags _ model =
    ( model, Cmd.none )
