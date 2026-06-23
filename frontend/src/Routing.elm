{- Client-side routing: maps URL paths to Page values defined in Types. -}
module Routing exposing (fromUrl, routeParser)

import Types exposing (Page(..))
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, top)


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map HomePage           top
        , Parser.map TimelinePage       (Parser.s "timeline")
        , Parser.map ImportantDaysPage  (Parser.s "important-days")
        , Parser.map NotesPage          (Parser.s "notes")
        , Parser.map PlansPage          (Parser.s "plans")
        , Parser.map DiaryPage          (Parser.s "diary")
        , Parser.map StatsPage          (Parser.s "stats")
        , Parser.map TrashPage          (Parser.s "trash")
        , Parser.map CalendarPage       (Parser.s "calendar")
        ]


fromUrl : Url -> Page
fromUrl url =
    Maybe.withDefault NotFoundPage (Parser.parse routeParser url)
