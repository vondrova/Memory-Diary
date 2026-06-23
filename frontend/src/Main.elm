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


type Page
    = HomePage
    | TimelinePage
    | NotFoundPage


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


type alias Model =
    { key : Nav.Key
    , page : Page
    , memories : List Memory
    , error : Maybe String
    , formOpen : Bool
    , formTitle : String
    , formTimeFrom : String
    , formTimeTo : String
    , formDescription : String
    , formLocation : String
    , formTags : String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotMemories (Result Http.Error (List Memory))
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


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map HomePage top
        , Parser.map TimelinePage (Parser.s "timeline")
        ]


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault NotFoundPage (Parser.parse routeParser url)


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , page = fromUrl url
      , memories = []
      , error = Nothing
      , formOpen = False
      , formTitle = ""
      , formTimeFrom = ""
      , formTimeTo = ""
      , formDescription = ""
      , formLocation = ""
      , formTags = ""
      }
    , fetchMemories
    )


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


fetchMemories : Cmd Msg
fetchMemories =
    Http.get
        { url = "/api/memories"
        , expect = Http.expectJson GotMemories (D.list memoryDecoder)
        }


createMemory : Model -> Cmd Msg
createMemory model =
    Http.post
        { url = "/api/memories"
        , body = Http.jsonBody (encodeMemoryInput model)
        , expect = Http.expectJson MemoryCreated memoryDecoder
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
            div []
                [ div [ class "page-actions" ]
                    [ button [ class "btn-primary", onClick OpenForm ] [ text "+ Přidat vzpomínku" ] ]
                , if model.formOpen then
                    viewForm model

                  else
                    text ""
                , div [ class "memory-list" ]
                    (List.map viewMemory model.memories)
                ]

        TimelinePage ->
            div [ class "timeline" ]
                (List.map viewTimelineItem model.memories)

        NotFoundPage ->
            div [ class "not-found" ] [ text "Stránka nenalezena." ]


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
                p [ class "memory-location" ] [ text loc ]

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
