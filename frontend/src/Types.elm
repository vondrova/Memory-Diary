module Types exposing
    ( Memory
    , ImportantDay
    , CoupleNote
    , CouplePlan
    , DiaryEntry
    , GeoSuggestion
    , Stats
    , Lang(..)
    , Theme(..)
    , themeClass
    )


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


type alias CoupleNote =
    { id : Maybe Int
    , owner : String
    , title : String
    , body : String
    , createdAt : String
    }


type alias CouplePlan =
    { id : Maybe Int
    , category : String
    , title : String
    , detail : Maybe String
    , done : Bool
    }


type alias DiaryEntry =
    { id : Maybe Int
    , date : String
    , mood : Maybe String
    , body : String
    , weather : Maybe String
    }


type alias GeoSuggestion =
    { displayName : String
    , lat : String
    , lon : String
    }


type alias Stats =
    { totalMinutes : Int
    , memoryCount : Int
    , photoCount : Int
    , visitedPlaces : Int
    , averageMinutes : Int
    , topTags : List ( String, Int )
    , topLocations : List ( String, Int )
    }


type Lang
    = CZ
    | EN


type Theme
    = Light
    | Dark


themeClass : Theme -> String
themeClass t =
    case t of
        Light ->
            "theme-light"

        Dark ->
            "theme-dark"
