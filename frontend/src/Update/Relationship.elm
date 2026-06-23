module Update.Relationship exposing (updateRelationship)

import Types exposing (..)


updateRelationship : Msg -> Model -> ( Model, Cmd Msg )
updateRelationship msg model =
    case msg of
        GotStats (Ok s) ->
            ( { model | stats = Just s }, Cmd.none )

        GotStats (Err _) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
