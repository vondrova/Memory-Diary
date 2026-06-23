module Utils.Helpers exposing (..)

{-| Shared pure helpers used across multiple modules
-}

import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Http
import Language exposing (countdownDays, tr)
import Types exposing (..)
import Utils.DateUtils exposing (dateParts, dayKey)



-- FILTER SETTERS -----------------------------
-- | Update the search query field in a Filter record


setFQ : String -> Filter -> Filter
setFQ v f =
    { f | q = v }



-- | Update the tag field in a Filter record


setFTag : String -> Filter -> Filter
setFTag v f =
    { f | tag = v }



-- | Update the location field in a Filter record


setFLoc : String -> Filter -> Filter
setFLoc v f =
    { f | location = v }



-- | Update the dateFrom field in a Filter record


setFFrom : String -> Filter -> Filter
setFFrom v f =
    { f | dateFrom = v }



-- | Update the dateTo field in a Filter record


setFTo : String -> Filter -> Filter
setFTo v f =
    { f | dateTo = v }



-- STATUS HELPERS -----------------------------
-- | Apply a function to the value inside a Loaded status, leaves Loading/Failed unchanged


mapStatus : (a -> a) -> Status a -> Status a
mapStatus f s =
    case s of
        Loaded x ->
            Loaded (f x)

        other ->
            other



-- | Extract the list from a Loaded status, or return [] for Loading/Failed


statusList : Status (List a) -> List a
statusList s =
    case s of
        Loaded xs ->
            xs

        _ ->
            []



-- AVATAR HELPERS -----------------------------
-- | Convert the API-level ProfileData into the frontend AvatarProfile


profileDataToAvatar : ProfileData -> AvatarProfile
profileDataToAvatar p =
    { name = p.name
    , mode = p.displayMode
    , photo = Maybe.withDefault "" p.photo
    , figureColor = p.figureColor
    , accessory = p.accessory
    , expression = p.expression
    , birthday = Maybe.withDefault "" p.birthday |> String.left 10
    }



-- | Return the left or right AvatarProfile from the model


avatarBySide : AvatarSide -> Model -> AvatarProfile
avatarBySide side model =
    case side of
        LeftAvatar ->
            model.leftAvatar

        RightAvatar ->
            model.rightAvatar



-- HTTP ERROR -----------------------------
-- | Convert an Http.Error to a short human-readable string for error display


httpErr : Http.Error -> String
httpErr e =
    case e of
        Http.BadUrl u ->
            "Bad URL: " ++ u

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus c ->
            "HTTP " ++ String.fromInt c

        Http.BadBody b ->
            "Invalid response: " ++ b



-- FORMATTING -----------------------------
-- | Format a from-to pair as "DD. MM. YYYY HH:MM - DD. MM. YYYY HH:MM"


formatTimeRange : String -> String -> String
formatTimeRange from to =
    formatDateTime from ++ " - " ++ formatDateTime to



-- | Format an ISO-8601 datetime string to "DD. MM. YYYY HH:MM"


formatDateTime : String -> String
formatDateTime value =
    let
        y =
            String.slice 0 4 value

        m =
            String.slice 5 7 value

        d =
            String.slice 8 10 value

        time =
            String.slice 11 16 value
    in
    d ++ ". " ++ m ++ ". " ++ y ++ " " ++ time



-- | Format a "YYYY-MM-DD" or ISO-8601 string to "DD. MM. YYYY"


formatDate : String -> String
formatDate s =
    let
        parts =
            String.split "-" (String.left 10 s)
    in
    case parts of
        [ y, m, d ] ->
            d ++ ". " ++ m ++ ". " ++ y

        _ ->
            String.left 10 s



-- | Format a Today record as "DD. MM. YYYY"


formatDateParts : Today -> String
formatDateParts d =
    dayKey d.year d.month d.day
        |> formatDate



-- | Format a duration in minutes as "X d Y h Z min"


formatDuration : Int -> String
formatDuration minutes =
    let
        days =
            minutes // (60 * 24)

        hours =
            modBy 24 (minutes // 60)

        mins =
            modBy 60 minutes
    in
    (if days > 0 then
        String.fromInt days ++ " d "

     else
        ""
    )
        ++ (if hours > 0 then
                String.fromInt hours ++ " h "

            else
                ""
           )
        ++ String.fromInt mins
        ++ " min"



-- | Build the URL for a stored photo filename


photoUrl : String -> String
photoUrl name =
    "/photos/" ++ name



-- | MIME types and extensions accepted by the file picker for photos


photoAcceptList : List String
photoAcceptList =
    [ "image/jpeg"
    , "image/png"
    , "image/webp"
    , "image/gif"
    , "image/bmp"
    , "image/tiff"
    , "image/heic"
    , "image/heif"
    , "image/avif"
    , ".jpg"
    , ".jpeg"
    , ".png"
    , ".webp"
    , ".gif"
    , ".bmp"
    , ".tif"
    , ".tiff"
    , ".heic"
    , ".heif"
    , ".avif"
    ]



-- | True when the filename has a known image extension and no path traversal characters


isSafePhotoName : String -> Bool
isSafePhotoName raw =
    let
        name =
            String.trim raw

        lower =
            String.toLower name

        allowedExt =
            List.any (\ext -> String.endsWith ext lower)
                [ ".jpg"
                , ".jpeg"
                , ".png"
                , ".webp"
                , ".gif"
                , ".bmp"
                , ".tif"
                , ".tiff"
                , ".heic"
                , ".heif"
                , ".avif"
                ]
    in
    not (String.isEmpty name)
        && name
        == raw
        && allowedExt
        && not (String.contains "/" name)
        && not (String.contains "\\" name)
        && not (String.contains ".." name)
        && not (String.contains "?" name)
        && not (String.contains "#" name)
        && not (String.contains ":" name)



-- | Build a Google Maps search URL for the given location string


mapsUrl : String -> String
mapsUrl loc =
    "https://www.google.com/maps/search/?api=1&query=" ++ String.replace " " "+" (String.trim loc)



-- | True when title and date are non-blank


importantDayFormValid : ImportantDayForm -> Bool
importantDayFormValid form =
    not (isBlank form.title) && not (isBlank form.date)



-- | True when owner is valid and title/body are non-blank


noteFormValid : NoteForm -> Bool
noteFormValid form =
    List.member form.owner [ "shared", "left", "right" ]
        && not (isBlank form.title)
        && not (isBlank form.body)



-- | True when category and title are non-blank


planFormValid : PlanForm -> Bool
planFormValid form =
    not (isBlank form.category) && not (isBlank form.title)



-- | Remove duplicates from a string list while preserving first-occurrence order


uniqueStrings : List String -> List String
uniqueStrings values =
    List.foldl
        (\value acc ->
            if List.member value acc then
                acc

            else
                value :: acc
        )
        []
        values



-- | Extract the start date (YYYY-MM-DD) from the loaded relationship, or ""


relationshipStartDate : Status RelationshipData -> String
relationshipStartDate relationship =
    case relationship of
        Loaded rel ->
            Maybe.withDefault "" (Maybe.map (String.left 10) rel.startDate)

        _ ->
            ""



-- ZODIAC -----------------------------
-- | Unicode zodiac symbol for a birthday date string


zodiacSymbol : String -> String
zodiacSymbol birthday =
    zodiac birthday
        |> Tuple.first



-- | English zodiac sign name for a birthday date string


zodiacName : String -> String
zodiacName birthday =
    zodiac birthday
        |> Tuple.second



-- | Look up (symbol, name) from a birthday date; returns ("·", "") when unparseable


zodiac : String -> ( String, String )
zodiac birthday =
    case dateParts birthday of
        Nothing ->
            ( "·", "" )

        Just d ->
            if ( d.month, d.day ) >= ( 3, 21 ) && ( d.month, d.day ) <= ( 4, 19 ) then
                ( "♈", "Aries" )

            else if ( d.month, d.day ) >= ( 4, 20 ) && ( d.month, d.day ) <= ( 5, 20 ) then
                ( "♉", "Taurus" )

            else if ( d.month, d.day ) >= ( 5, 21 ) && ( d.month, d.day ) <= ( 6, 20 ) then
                ( "♊", "Gemini" )

            else if ( d.month, d.day ) >= ( 6, 21 ) && ( d.month, d.day ) <= ( 7, 22 ) then
                ( "♋", "Cancer" )

            else if ( d.month, d.day ) >= ( 7, 23 ) && ( d.month, d.day ) <= ( 8, 22 ) then
                ( "♌", "Leo" )

            else if ( d.month, d.day ) >= ( 8, 23 ) && ( d.month, d.day ) <= ( 9, 22 ) then
                ( "♍", "Virgo" )

            else if ( d.month, d.day ) >= ( 9, 23 ) && ( d.month, d.day ) <= ( 10, 22 ) then
                ( "♎", "Libra" )

            else if ( d.month, d.day ) >= ( 10, 23 ) && ( d.month, d.day ) <= ( 11, 21 ) then
                ( "♏", "Scorpio" )

            else if ( d.month, d.day ) >= ( 11, 22 ) && ( d.month, d.day ) <= ( 12, 21 ) then
                ( "♐", "Sagittarius" )

            else if ( d.month, d.day ) >= ( 12, 22 ) || ( d.month, d.day ) <= ( 1, 19 ) then
                ( "♑", "Capricorn" )

            else if ( d.month, d.day ) >= ( 1, 20 ) && ( d.month, d.day ) <= ( 2, 18 ) then
                ( "♒", "Aquarius" )

            else
                ( "♓", "Pisces" )



-- CALENDAR HELPERS -----------------------------
-- | Filter memories whose timeFrom starts with the given dayKey string


memoriesOnDay : List Memory -> String -> List Memory
memoriesOnDay mems key =
    List.filter (\m -> String.startsWith key m.timeFrom) mems



-- | Important days that fall on the given calendar day


importantDaysOnCalendarDay : Int -> Int -> String -> List ImportantDay -> List ImportantDay
importantDaysOnCalendarDay calY calM key anns =
    let
        day =
            String.slice 8 10 key |> String.toInt |> Maybe.withDefault 1
    in
    List.filter (importantDayOccursOn calY calM day) anns



-- | True when the important day's month and day match the given calendar cell


importantDayOccursOn : Int -> Int -> Int -> ImportantDay -> Bool
importantDayOccursOn _ calM day ann =
    ann.month == calM && ann.day == day



-- | "Today!", "Tomorrow", or "X days" depending on how many days until the event


countdownShort : Language -> Int -> String
countdownShort lang days =
    if days == 0 then
        tr lang "Today!"

    else if days == 1 then
        tr lang "Tomorrow"

    else
        countdownDays lang days



-- ANNIVERSARY DISPLAY -----------------------------
-- | Prefixes birthday events with "birthday: name"; leaves other titles unchanged


displayImportantDayTitle : Language -> ImportantDay -> String
displayImportantDayTitle lang ann =
    if ann.kind == "birthday" then
        birthdayEventName lang ann.title

    else
        ann.title



-- | Localized "birthday: name" string for a birthday event


birthdayEventName : Language -> String -> String
birthdayEventName lang name =
    case lang of
        English ->
            "birthday: " ++ name

        Czech ->
            tr lang "birthday" ++ ": " ++ name



-- | Translate an ordinal event title like "3. anniversary" to the current language


translateOrdinalEventName : Language -> String -> String -> String
translateOrdinalEventName lang eventName name =
    let
        parts =
            String.split " " name
    in
    case parts of
        number :: _ ->
            if String.endsWith "." number then
                number ++ " " ++ tr lang eventName

            else
                tr lang eventName

        [] ->
            tr lang eventName



-- FORM VALIDATION HELPERS -----------------------------
-- | True when the string is empty or only whitespace


isBlank : String -> Bool
isBlank value =
    String.isEmpty (String.trim value)



-- | CSS class for a required field, adds invalid-field when validation is shown and the field is empty


requiredClass : Bool -> Bool -> String
requiredClass showValidation invalid =
    if showValidation && invalid then
        "field required-field invalid-field"

    else
        "field required-field"



-- | Like requiredClass but without required-field, used for datetime row groups


formGroupClass : Bool -> Bool -> String
formGroupClass showValidation invalid =
    if showValidation && invalid then
        "form-group invalid-field"

    else
        "form-group"



-- | Red "fill in required fields" paragraph shown when validation fails


validationNotice : Language -> Bool -> Bool -> Html Msg
validationNotice lang showValidation isValid =
    if showValidation && not isValid then
        p [ class "validation-message" ] [ text (tr lang "Please fill in all required fields marked with the pink dot.") ]

    else
        text ""



-- | Count distinct trimmed-lowercase place names across all memories


visitedPlacesCount : List Memory -> Int
visitedPlacesCount memories =
    memories
        |> List.filterMap .location
        |> List.map (String.trim >> String.toLower)
        |> List.filter (\place -> not (String.isEmpty place))
        |> uniqueStrings
        |> List.length



-- HTTP MUTATION WORKFLOW -----------------------------
-- | Map an Http.Error to a stable user-facing error message


mutationFailure : Http.Error -> String
mutationFailure error =
    case error of
        Http.BadUrl _ ->
            "The request URL is invalid."

        Http.Timeout ->
            "The request timed out. Please try again."

        Http.NetworkError ->
            "The server is unreachable. Check your connection and try again."

        Http.BadStatus _ ->
            "The server rejected the request."

        Http.BadBody _ ->
            "The server returned an invalid response."



-- | Store a mutationFailure message in model.infoDialog


withMutationFailure : Http.Error -> { state | infoDialog : Maybe String } -> { state | infoDialog : Maybe String }
withMutationFailure error state =
    { state | infoDialog = Just (mutationFailure error) }
