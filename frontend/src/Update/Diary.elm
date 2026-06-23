module Update.Diary exposing (updateDiary)

import Api.Requests as Requests
import Types exposing (..)


updateDiary : Msg -> Model -> ( Model, Cmd Msg )
updateDiary msg model =
    case msg of
        GotDiary (Ok entries) ->
            ( { model | diaryEntries = entries }, Cmd.none )

        GotDiary (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst deník." }, Cmd.none )

        SetDiaryDate v ->
            ( { model | newDiaryDate = v }, Cmd.none )

        SetDiaryBody v ->
            ( { model | newDiaryBody = v }, Cmd.none )

        SetDiaryMood v ->
            ( { model | newDiaryMood = v }, Cmd.none )

        SubmitDiaryEntry ->
            ( model
            , Requests.createDiaryEntry
                { date = model.newDiaryDate, mood = model.newDiaryMood, body = model.newDiaryBody }
                DiaryCreated
            )

        DiaryCreated (Ok entry) ->
            ( { model
                | diaryEntries = entry :: model.diaryEntries
                , newDiaryDate = ""
                , newDiaryBody = ""
                , newDiaryMood = "😊"
              }
            , Cmd.none
            )

        DiaryCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit zápis." }, Cmd.none )

        _ ->
            ( model, Cmd.none )
