module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, top)


-- PAGES

type Page
    = HomePage
    | TimelinePage
    | ImportantDaysPage
    | StatsPage
    | NotFoundPage


-- DOMAIN TYPES

type alias Memory =
    { id : Int
    , title : String
    , timeFrom : String
    , timeTo : String
    , description : Maybe String
    , location : Maybe String
    , tags : List String
    , photos : List String
    }


type alias ImportantDay =
    { id : Maybe Int
    , title : String
    , date : String
    , note : Maybe String
    , kind : String
    }


type alias Model =
    { key : Nav.Key
    , page : Page
    , memories : List Memory
    , importantDays : List ImportantDay
    , searchQuery : String
    , error : Maybe String
    , formOpen : Bool
    , formTitle : String
    , formTimeFrom : String
    , formTimeTo : String
    , formDescription : String
    , formLocation : String
    , formTags : String
    , newDayTitle : String
    , newDayDate : String
    , newDayKind : String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotMemories (Result Http.Error (List Memory))
    | GotImportantDays (Result Http.Error (List ImportantDay))
    | SetSearch String
    | OpenForm
    | CloseForm
    | SetTitle String
    | SetTimeFrom String
    | SetTimeTo String
    | SetDescription String
    | SetLocation String
    | SetTags String
    | SubmitForm
    | MemoryCreated (Result Http.Error Memory)
    | SetNewDayTitle String
    | SetNewDayDate String
    | SetNewDayKind String
    | SubmitNewDay
    | DayCreated (Result Http.Error ImportantDay)


-- ROUTING

routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map HomePage top
        , Parser.map TimelinePage (Parser.s "timeline")
        , Parser.map ImportantDaysPage (Parser.s "important-days")
        , Parser.map StatsPage (Parser.s "stats")
        ]


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault NotFoundPage (Parser.parse routeParser url)


-- INIT

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , page = fromUrl url
      , memories = []
      , importantDays = []
      , searchQuery = ""
      , error = Nothing
      , formOpen = False
      , formTitle = ""
      , formTimeFrom = ""
      , formTimeTo = ""
      , formDescription = ""
      , formLocation = ""
      , formTags = ""
      , newDayTitle = ""
      , newDayDate = ""
      , newDayKind = "anniversary"
      }
    , Cmd.batch [ fetchMemories, fetchImportantDays ]
    )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            ( { model | page = fromUrl url }, Cmd.none )

        GotMemories (Ok mems) ->
            ( { model | memories = mems, error = Nothing }, Cmd.none )

        GotMemories (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst vzpomínky." }, Cmd.none )

        GotImportantDays (Ok days) ->
            ( { model | importantDays = days }, Cmd.none )

        GotImportantDays (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst důležité dny." }, Cmd.none )

        SetSearch q ->
            ( { model | searchQuery = q }, Cmd.none )

        OpenForm ->
            ( { model | formOpen = True }, Cmd.none )

        CloseForm ->
            ( { model | formOpen = False }, Cmd.none )

        SetTitle v ->
            ( { model | formTitle = v }, Cmd.none )

        SetTimeFrom v ->
            ( { model | formTimeFrom = v }, Cmd.none )

        SetTimeTo v ->
            ( { model | formTimeTo = v }, Cmd.none )

        SetDescription v ->
            ( { model | formDescription = v }, Cmd.none )

        SetLocation v ->
            ( { model | formLocation = v }, Cmd.none )

        SetTags v ->
            ( { model | formTags = v }, Cmd.none )

        SubmitForm ->
            ( model, createMemory model )

        MemoryCreated (Ok mem) ->
            ( { model
                | memories = model.memories ++ [ mem ]
                , formOpen = False
                , formTitle = ""
                , formTimeFrom = ""
                , formTimeTo = ""
                , formDescription = ""
                , formLocation = ""
                , formTags = ""
              }
            , Cmd.none
            )

        MemoryCreated (Err _) ->
            ( { model | error = Just "Nepodařilo se uložit vzpomínku." }, Cmd.none )

        SetNewDayTitle v ->
            ( { model | newDayTitle = v }, Cmd.none )

        SetNewDayDate v ->
            ( { model | newDayDate = v }, Cmd.none )

        SetNewDayKind v ->
            ( { model | newDayKind = v }, Cmd.none )

        SubmitNewDay ->
            ( model, createImportantDay model )

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


-- HTTP

fetchMemories : Cmd Msg
fetchMemories =
    Http.get
        { url = "/api/memories"
        , expect = Http.expectJson GotMemories (D.list memoryDecoder)
        }


fetchImportantDays : Cmd Msg
fetchImportantDays =
    Http.get
        { url = "/api/important-days"
        , expect = Http.expectJson GotImportantDays (D.list importantDayDecoder)
        }


createMemory : Model -> Cmd Msg
createMemory model =
    Http.post
        { url = "/api/memories"
        , body = Http.jsonBody (encodeMemoryInput model)
        , expect = Http.expectJson MemoryCreated memoryDecoder
        }


createImportantDay : Model -> Cmd Msg
createImportantDay model =
    Http.post
        { url = "/api/important-days"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "importantDayInputTitle", E.string model.newDayTitle )
                    , ( "importantDayInputDate", E.string model.newDayDate )
                    , ( "importantDayInputNote", E.null )
                    , ( "importantDayInputKind", E.string model.newDayKind )
                    ]
                )
        , expect = Http.expectJson DayCreated importantDayDecoder
        }


encodeMemoryInput : Model -> E.Value
encodeMemoryInput model =
    let
        tags =
            model.formTags
                |> String.split ","
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        nullable s =
            if String.isEmpty s then
                E.null
            else
                E.string s
    in
    E.object
        [ ( "title", E.string model.formTitle )
        , ( "timeFrom", E.string model.formTimeFrom )
        , ( "timeTo", E.string model.formTimeTo )
        , ( "description", nullable model.formDescription )
        , ( "location", nullable model.formLocation )
        , ( "tags", E.list E.string tags )
        , ( "photos", E.list E.string [] )
        ]


-- DECODERS

memoryDecoder : D.Decoder Memory
memoryDecoder =
    D.map8 Memory
        (D.field "memoryId" D.int)
        (D.field "memoryTitle" D.string)
        (D.field "memoryTimeFrom" D.string)
        (D.field "memoryTimeTo" D.string)
        (D.maybe (D.field "memoryDescription" D.string))
        (D.maybe (D.field "memoryLocation" D.string))
        (D.field "memoryTags" (D.list D.string))
        (D.field "memoryPhotos" (D.list D.string))


importantDayDecoder : D.Decoder ImportantDay
importantDayDecoder =
    D.map5 ImportantDay
        (D.maybe (D.field "importantDayId" D.int))
        (D.field "importantDayTitle" D.string)
        (D.field "importantDayDate" D.string)
        (D.maybe (D.field "importantDayNote" D.string))
        (D.field "importantDayKind" D.string)


-- BUSINESS LOGIC ON FRONTEND (should eventually move to backend)

filterMemories : String -> List Memory -> List Memory
filterMemories query mems =
    if String.isEmpty query then
        mems
    else
        let
            q = String.toLower query
        in
        List.filter
            (\m ->
                String.contains q (String.toLower m.title)
                    || List.any (String.contains q << String.toLower) m.tags
                    || Maybe.withDefault False (Maybe.map (String.contains q << String.toLower) m.location)
                    || Maybe.withDefault False (Maybe.map (String.contains q << String.toLower) m.description)
            )
            mems


countTags : List Memory -> List ( String, Int )
countTags mems =
    let
        allTags =
            List.concatMap .tags mems

        increment tag acc =
            case List.partition (\( k, _ ) -> k == tag) acc of
                ( [], rest ) ->
                    ( tag, 1 ) :: rest

                ( ( k, n ) :: _, rest ) ->
                    ( k, n + 1 ) :: rest
    in
    List.foldl increment [] allTags
        |> List.sortBy (negate << Tuple.second)


parseDatetimeMinutes : String -> Maybe Int
parseDatetimeMinutes s =
    case String.split "T" s of
        [ datePart, timePart ] ->
            case ( String.split "-" datePart, String.split ":" timePart ) of
                ( [ y, mo, d ], [ h, mi ] ) ->
                    Maybe.map5
                        (\year month day hour minute ->
                            year * 365 * 24 * 60
                                + month * 30 * 24 * 60
                                + day * 24 * 60
                                + hour * 60
                                + minute
                        )
                        (String.toInt y)
                        (String.toInt mo)
                        (String.toInt d)
                        (String.toInt h)
                        (String.toInt mi)

                _ ->
                    Nothing

        _ ->
            Nothing


durationMinutes : Memory -> Int
durationMinutes mem =
    case ( parseDatetimeMinutes mem.timeFrom, parseDatetimeMinutes mem.timeTo ) of
        ( Just f, Just t ) ->
            Basics.max 0 (t - f)

        _ ->
            0


computeStats : List Memory -> { total : Int, count : Int, places : Int, avgMins : Int }
computeStats mems =
    let
        total =
            List.sum (List.map durationMinutes mems)

        places =
            mems
                |> List.filterMap .location
                |> List.filter (not << String.isEmpty)
                |> List.sort
                |> dedupe
                |> List.length

        avg =
            if List.isEmpty mems then 0 else total // List.length mems
    in
    { total = total, count = List.length mems, places = places, avgMins = avg }


dedupe : List String -> List String
dedupe xs =
    List.foldl
        (\x acc ->
            if List.member x acc then acc else acc ++ [ x ]
        )
        []
        xs


daysUntilFromDate : String -> String -> Int
daysUntilFromDate todayStr dateStr =
    let
        todayMD = String.slice 5 10 todayStr
        dateMD  = String.slice 5 10 dateStr
    in
    if dateMD >= todayMD then
        0
    else
        365


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Memory Diary"
    , body =
        [ div [ class "app" ]
            [ viewHeader model.page
            , main_ [ class "main" ]
                [ case model.error of
                    Just err ->
                        div [ class "error" ] [ text err ]

                    Nothing ->
                        text ""
                , viewPage model
                ]
            ]
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    header [ class "header" ]
        [ h1 [ class "header-title" ] [ text "Memory Diary" ]
        , nav [ class "nav" ]
            [ navLink "/" "Vzpomínky" (page == HomePage)
            , navLink "/timeline" "Timeline" (page == TimelinePage)
            , navLink "/important-days" "Důležité dny" (page == ImportantDaysPage)
            , navLink "/stats" "Statistiky" (page == StatsPage)
            ]
        ]


navLink : String -> String -> Bool -> Html Msg
navLink href_ label_ active =
    a
        [ href href_
        , class
            (if active then
                "nav-link nav-link--active"
             else
                "nav-link"
            )
        ]
        [ text label_ ]


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        HomePage ->
            viewHome model

        TimelinePage ->
            div [ class "timeline" ]
                (List.map viewTimelineItem model.memories)

        ImportantDaysPage ->
            viewImportantDays model

        StatsPage ->
            viewStats model

        NotFoundPage ->
            div [ class "not-found" ] [ text "Stránka nenalezena." ]


viewHome : Model -> Html Msg
viewHome model =
    let
        filtered =
            filterMemories model.searchQuery model.memories
    in
    div []
        [ div [ class "page-actions" ]
            [ input
                [ type_ "search"
                , placeholder "Hledat vzpomínky…"
                , value model.searchQuery
                , onInput SetSearch
                , class "search-input"
                ]
                []
            , button [ class "btn-primary", onClick OpenForm ] [ text "+ Přidat vzpomínku" ]
            ]
        , if model.formOpen then
            viewForm model
          else
            text ""
        , if List.isEmpty filtered then
            p [ class "empty-state" ] [ text "Žádné vzpomínky nenalezeny." ]
          else
            div [ class "memory-list" ] (List.map viewMemory filtered)
        ]


viewForm : Model -> Html Msg
viewForm model =
    div [ class "form-overlay" ]
        [ Html.form [ class "memory-form", onSubmit SubmitForm ]
            [ h2 [] [ text "Nová vzpomínka" ]
            , label [] [ text "Název" ]
            , input [ type_ "text", value model.formTitle, onInput SetTitle, required True ] []
            , label [] [ text "Od" ]
            , input [ type_ "datetime-local", value model.formTimeFrom, onInput SetTimeFrom, required True ] []
            , label [] [ text "Do" ]
            , input [ type_ "datetime-local", value model.formTimeTo, onInput SetTimeTo, required True ] []
            , label [] [ text "Místo" ]
            , input [ type_ "text", value model.formLocation, onInput SetLocation ] []
            , label [] [ text "Tagy (oddělené čárkou)" ]
            , input [ type_ "text", value model.formTags, onInput SetTags ] []
            , label [] [ text "Popis" ]
            , textarea [ value model.formDescription, onInput SetDescription ] []
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text "Uložit" ]
                , button [ type_ "button", class "btn-secondary", onClick CloseForm ] [ text "Zrušit" ]
                ]
            ]
        ]


viewMemory : Memory -> Html Msg
viewMemory mem =
    div [ class "memory-card" ]
        [ h2 [ class "memory-title" ] [ text mem.title ]
        , p [ class "memory-time" ] [ text (mem.timeFrom ++ " – " ++ mem.timeTo) ]
        , case mem.location of
            Just loc ->
                p [ class "memory-location" ] [ text ("📍 " ++ loc) ]

            Nothing ->
                text ""
        , case mem.description of
            Just desc ->
                p [ class "memory-desc" ] [ text desc ]

            Nothing ->
                text ""
        , div [ class "memory-tags" ]
            (List.map (\t -> span [ class "tag" ] [ text t ]) mem.tags)
        ]


viewTimelineItem : Memory -> Html Msg
viewTimelineItem mem =
    div [ class "timeline-item" ]
        [ div [ class "timeline-dot" ] []
        , div [ class "timeline-content" ]
            [ span [ class "timeline-date" ] [ text mem.timeFrom ]
            , strong [] [ text mem.title ]
            , case mem.location of
                Just loc ->
                    span [ class "timeline-location" ] [ text (" · " ++ loc) ]

                Nothing ->
                    text ""
            ]
        ]


viewImportantDays : Model -> Html Msg
viewImportantDays model =
    div []
        [ Html.form [ class "memory-form", onSubmit SubmitNewDay, style "margin-bottom" "1.5rem" ]
            [ h2 [] [ text "Přidat důležitý den" ]
            , label [] [ text "Název" ]
            , input [ type_ "text", value model.newDayTitle, onInput SetNewDayTitle, required True ] []
            , label [] [ text "Datum" ]
            , input [ type_ "date", value model.newDayDate, onInput SetNewDayDate, required True ] []
            , label [] [ text "Typ" ]
            , select [ onInput SetNewDayKind ]
                [ option [ value "anniversary", selected (model.newDayKind == "anniversary") ] [ text "Výročí" ]
                , option [ value "birthday", selected (model.newDayKind == "birthday") ] [ text "Narozeniny" ]
                , option [ value "other", selected (model.newDayKind == "other") ] [ text "Jiné" ]
                ]
            , div [ class "form-actions" ]
                [ button [ type_ "submit", class "btn-primary" ] [ text "Uložit" ] ]
            ]
        , if List.isEmpty model.importantDays then
            p [ class "empty-state" ] [ text "Žádné důležité dny." ]
          else
            div [ class "memory-list" ]
                (List.map viewImportantDay
                    (List.sortBy .date model.importantDays)
                )
        ]


viewImportantDay : ImportantDay -> Html Msg
viewImportantDay day =
    let
        kindLabel =
            case day.kind of
                "birthday"    -> "🎂 Narozeniny"
                "anniversary" -> "💍 Výročí"
                _             -> "📅 Jiné"
    in
    div [ class "memory-card" ]
        [ h2 [ class "memory-title" ] [ text day.title ]
        , p [ class "memory-time" ] [ text (kindLabel ++ "  ·  " ++ day.date) ]
        , case day.note of
            Just n  -> p [ class "memory-desc" ] [ text n ]
            Nothing -> text ""
        ]


viewStats : Model -> Html Msg
viewStats model =
    let
        stats =
            computeStats model.memories

        topTags =
            List.take 5 (countTags model.memories)
    in
    div [ class "stats-page" ]
        [ h2 [] [ text "Statistiky" ]
        , div [ class "stats-grid" ]
            [ statCard "Vzpomínek" (String.fromInt stats.count)
            , statCard "Celkem minut" (String.fromInt stats.total)
            , statCard "Průměr minut" (String.fromInt stats.avgMins)
            , statCard "Navštívená místa" (String.fromInt stats.places)
            ]
        , h3 [] [ text "Nejčastější tagy" ]
        , if List.isEmpty topTags then
            p [] [ text "Žádné tagy." ]
          else
            ul [ class "tag-list" ]
                (List.map
                    (\( tag, count ) ->
                        li [] [ span [ class "tag" ] [ text tag ], text (" × " ++ String.fromInt count) ]
                    )
                    topTags
                )
        ]


statCard : String -> String -> Html Msg
statCard label value =
    div [ class "stat-card" ]
        [ div [ class "stat-value" ] [ text value ]
        , div [ class "stat-label" ] [ text label ]
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
