module Update.ImportantDays exposing (updateImportantDays)

import Api.Requests as Requests
import Types exposing (..)


updateImportantDays : Msg -> Model -> ( Model, Cmd Msg )
updateImportantDays msg model =
    case msg of
        GotImportantDays (Ok days) ->
            ( { model | importantDays = days }, Cmd.none )

        GotImportantDays (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst důležité dny." }, Cmd.none )

        SetNewDayTitle v ->
            ( { model | newDayTitle = v }, Cmd.none )

        SetNewDayDate v ->
            ( { model | newDayDate = v }, Cmd.none )

        SetNewDayKind v ->
            ( { model | newDayKind = v }, Cmd.none )

        SubmitNewDay ->
            ( model
            , Requests.createImportantDay
                { title = model.newDayTitle, date = model.newDayDate, kind = model.newDayKind }
                DayCreated
            )

        DayCreated (Ok day) ->
            ( { model
                | importantDays = model.importantDays ++ [ day ]
                , newDayTitle = ""
                , newDayDate = ""
                , newDayKind = "anniversary"
              }
            , Cmd.none
            )

        DayCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit důležitý den." }, Cmd.none )

        _ ->
            ( model, Cmd.none )
