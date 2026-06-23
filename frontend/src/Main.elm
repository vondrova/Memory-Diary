module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E


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
    { memories : List Memory
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
    = GotMemories (Result Http.Error (List Memory))
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { memories = []
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
            [ header [ class "header" ]
                [ h1 [] [ text "Memory Diary" ]
                , button [ class "btn-primary", onClick OpenForm ] [ text "+ Přidat vzpomínku" ]
                ]
            , main_ [ class "main" ]
                [ case model.error of
                    Just err ->
                        div [ class "error" ] [ text err ]

                    Nothing ->
                        text ""
                , if model.formOpen then
                    viewForm model

                  else
                    text ""
                , div [ class "memory-list" ]
                    (List.map viewMemory model.memories)
                ]
            ]
        ]
    }


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


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
