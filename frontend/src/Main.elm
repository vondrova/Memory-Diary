module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D


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
    }


type Msg
    = GotMemories (Result Http.Error (List Memory))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { memories = [], error = Nothing }
    , fetchMemories
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMemories (Ok mems) ->
            ( { model | memories = mems, error = Nothing }, Cmd.none )

        GotMemories (Err _) ->
            ( { model | error = Just "Nepodařilo se načíst vzpomínky." }, Cmd.none )


fetchMemories : Cmd Msg
fetchMemories =
    Http.get
        { url = "/api/memories"
        , expect = Http.expectJson GotMemories (D.list memoryDecoder)
        }


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
                [ h1 [] [ text "Memory Diary" ] ]
            , main_ [ class "main" ]
                [ case model.error of
                    Just err ->
                        div [ class "error" ] [ text err ]

                    Nothing ->
                        div [ class "memory-list" ]
                            (List.map viewMemory model.memories)
                ]
            ]
        ]
    }


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
