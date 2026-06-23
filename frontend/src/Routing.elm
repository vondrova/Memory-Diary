module Routing exposing (Page(..), fromUrl, routeParser)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, top)


type Page
    = HomePage
    | TimelinePage
    | ImportantDaysPage
    | NotesPage
    | PlansPage
    | DiaryPage
    | StatsPage
    | TrashPage
    | NotFoundPage


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map HomePage top
        , Parser.map TimelinePage (Parser.s "timeline")
        , Parser.map ImportantDaysPage (Parser.s "important-days")
        , Parser.map NotesPage (Parser.s "notes")
        , Parser.map PlansPage (Parser.s "plans")
        , Parser.map DiaryPage (Parser.s "diary")
        , Parser.map StatsPage (Parser.s "stats")
        , Parser.map TrashPage (Parser.s "trash")
        ]


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault NotFoundPage (Parser.parse routeParser url)
