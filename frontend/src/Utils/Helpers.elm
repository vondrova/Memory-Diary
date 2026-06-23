module Utils.Helpers exposing (noCmd)

import Types exposing (Model, Msg)


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )
