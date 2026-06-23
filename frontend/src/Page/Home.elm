module Page.Home exposing (viewAvatarName, viewAvatarVisual, viewHome)

{-| Home page: couple overview, upcoming events, and dashboard panels.
-}

import Html exposing (Html, a, aside, br, button, div, em, h1, h2, h3, img, input, label, li, nav, option, p, section, select, small, span, strong, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Language exposing (tr)
import Routing exposing (pageUrlFor)
import Types exposing (..)
import Utils.DateUtils exposing (dateParts)
import Utils.Helpers
    exposing
        ( avatarBySide
        , birthdayEventName
        , countdownShort
        , formatDate
        , formatDateParts
        , photoUrl
        , translateOrdinalEventName
        , zodiacName
        , zodiacSymbol
        )


viewHome : Model -> Html Msg
viewHome model =
    div [ class "home-page" ]
        [ section [ class "home-hero" ]
            [ div []
                [ h1 [] [ text "Love Memory Diary" ]
                ]
            ]
        , viewCoupleSection model
        , viewHomeSnapshot model
        , viewHomeHighlights model
        ]



-- | Compact upcoming-event card linking to the important-days page


viewHomeSnapshot : Model -> Html Msg
viewHomeSnapshot model =
    let
        visibleDays =
            case model.importantDays of
                Loaded as_ ->
                    as_

                _ ->
                    []

        nextEvents =
            nearestUpcomingEvents model.today model.relationship visibleDays
    in
    div [ class "home-snapshot" ] [ viewUpcomingSnapshot model.lang nextEvents ]



-- | Renders the nearest event as a countdown, or a placeholder if none exist


viewUpcomingSnapshot : Language -> List UpcomingEvent -> Html Msg
viewUpcomingSnapshot lang events =
    case events of
        [] ->
            a [ class "home-snapshot-card", href (pageUrlFor lang ImportantDaysPage) ]
                [ strong [] [ text "—" ]
                , span [] [ text (tr lang "next date") ]
                ]

        first :: rest ->
            a [ class "home-snapshot-card", href (pageUrlFor lang ImportantDaysPage) ]
                [ strong [] [ text (countdownShort lang first.days) ]
                , div [ class "snapshot-event-list" ]
                    (List.map
                        (\event ->
                            span [] [ text (translateUpcomingEventTitle lang event ++ " · " ++ formatDateParts event.date) ]
                        )
                        (first :: rest)
                    )
                ]



-- | Three highlight panels: latest memories, open plans, and on-this-day memories


viewHomeHighlights : Model -> Html Msg
viewHomeHighlights model =
    let
        memories =
            case model.memories of
                Loaded ms ->
                    ms

                _ ->
                    []

        plans =
            case model.plans of
                Loaded ps ->
                    ps

                _ ->
                    []

        todayMemories =
            case model.onThisDay of
                Loaded ms ->
                    ms

                _ ->
                    []
    in
    section [ class "home-highlights" ]
        [ viewLatestMemoriesPanel model.lang memories
        , viewPlansPreview model.lang plans
        , viewOnThisDayPreview model.lang todayMemories
        ]



-- | Shows the three most recent memories, links to the timeline


viewLatestMemoriesPanel : Language -> List Memory -> Html Msg
viewLatestMemoriesPanel lang memories =
    let
        latest =
            memories |> List.reverse |> List.take 3
    in
    a [ class "home-panel home-panel-link memory-panel", href (pageUrlFor lang TimelinePage) ]
        [ div [ class "panel-title-row" ]
            [ h2 [] [ text (tr lang "Latest memories") ]
            , span [] [ text (String.fromInt (List.length memories)) ]
            ]
        , if List.isEmpty latest then
            p [ class "panel-empty" ] [ text (tr lang "Once you add your first memory, a small selection will appear here.") ]

          else
            div [ class "mini-list" ] (List.map viewMiniMemory latest)
        ]



-- | Compact memory card with thumbnail photo or a fallback heart icon


viewMiniMemory : Memory -> Html Msg
viewMiniMemory mem =
    div [ class "mini-memory" ]
        [ case List.head mem.photos of
            Just photo ->
                img [ src (photoUrl photo), alt "" ] []

            Nothing ->
                div [ class "mini-memory-fallback" ] [ text "♡" ]
        , div []
            [ h3 [] [ text mem.title ]
            , p [] [ text (formatDate mem.timeFrom) ]
            ]
        ]



-- | Shows up to three pending plans, links to the plans page


viewPlansPreview : Language -> List CouplePlan -> Html Msg
viewPlansPreview lang plans =
    let
        openPlans =
            plans
                |> List.filter (\plan -> not plan.done)
                |> List.take 3
    in
    a [ class "home-panel home-panel-link plans-panel", href (pageUrlFor lang PlansPage) ]
        [ div [ class "panel-title-row" ]
            [ h2 [] [ text (tr lang "Our plans") ]
            , span [] [ text (String.fromInt (List.length openPlans)) ]
            ]
        , if List.isEmpty openPlans then
            p [ class "panel-empty" ] [ text (tr lang "Upcoming ideas from Plans will appear here.") ]

          else
            div [ class "note-chip-list" ] (List.map viewPlanChip openPlans)
        ]



-- | Compact chip showing category and plan title


viewPlanChip : CouplePlan -> Html Msg
viewPlanChip plan =
    div [ class "note-chip" ]
        [ span [] [ text plan.category ]
        , strong [] [ text plan.title ]
        ]



-- | Shows memories matching today's day and month from past years, links to the timeline


viewOnThisDayPreview : Language -> List Memory -> Html Msg
viewOnThisDayPreview lang memories =
    a [ class "home-panel home-panel-link today-panel", href (pageUrlFor lang TimelinePage) ]
        [ div [ class "panel-title-row" ]
            [ h2 [] [ text (tr lang "On this day") ]
            , span [] [ text (String.fromInt (List.length memories)) ]
            ]
        , if List.isEmpty memories then
            p [ class "panel-empty" ] [ text (tr lang "When a memory matches today's day and month, it will be remembered here.") ]

          else
            div [ class "mini-list" ] (List.map viewMiniMemory (List.take 3 memories))
        ]



-- | Avatar cards, heart counter, and inline editors for the couple


viewCoupleSection : Model -> Html Msg
viewCoupleSection model =
    section [ class "couple-section" ]
        [ div [ class "couple-header" ]
            [ h2 [] [ text (tr model.lang "Us two") ] ]
        , div [ class "couple-stage" ]
            [ viewAvatarCard model.lang LeftAvatar model.leftAvatar
            , viewHeartCounter model.lang model.today model.relationship model.heartColor
            , viewAvatarCard model.lang RightAvatar model.rightAvatar
            ]
        , if model.editingHeart then
            viewHeartEditor model.lang model.relationshipDate model.heartColor

          else
            text ""
        , case model.editingAvatar of
            Nothing ->
                text ""

            Just side ->
                viewAvatarEditor model.lang
                    side
                    (Maybe.withDefault (avatarBySide side model) model.draftAvatar)
        ]



-- | Days-together counter, clicking it opens the heart editor


viewHeartCounter : Language -> Today -> Status RelationshipData -> String -> Html Msg
viewHeartCounter lang today relationship heartColor =
    case relationship of
        Loaded rel ->
            case rel.startDate of
                Nothing ->
                    emptyHeartCounter heartColor

                Just startDate ->
                    button [ class ("heart-counter " ++ heartColor), type_ "button", onClick OpenHeartEditor ]
                        [ span [] [ text (String.fromInt (Maybe.withDefault 0 rel.daysTogether)) ]
                        , small [] [ text (tr lang "days together") ]
                        , em []
                            [ text (formatDate startDate)
                            , br [] []
                            , text "-"
                            , br [] []
                            , text (formatDateParts today)
                            ]
                        ]

        _ ->
            emptyHeartCounter heartColor



-- | Placeholder heart shown before a start date is set


emptyHeartCounter : String -> Html Msg
emptyHeartCounter heartColor =
    button [ class ("heart-counter empty-heart " ++ heartColor), type_ "button", onClick OpenHeartEditor ]
        [ span [] [ text "" ]
        , small [] [ text "" ]
        ]



-- | Modal for editing the relationship start date and heart color


viewHeartEditor : Language -> String -> String -> Html Msg
viewHeartEditor lang relationshipDate heartColor =
    div [ class "modal-backdrop" ]
        [ div [ class "avatar-editor-modal heart-editor-modal" ]
            [ div [ class "form-header" ]
                [ h2 [] [ text (tr lang "Edit heart") ]
                , button [ class "btn-icon", onClick CloseHeartEditor ] [ text "✕" ]
                ]
            , div [ class ("heart-editor-preview heart-counter " ++ heartColor) ]
                [ span [] [ text "" ]
                ]
            , div [ class "form-row" ]
                [ avatarSelect (tr lang "Heart color")
                    heartColor
                    SetHeartColor
                    [ ( "heart-red", tr lang "Red" ), ( "heart-pink", tr lang "Pink" ), ( "heart-purple", tr lang "Purple" ), ( "heart-blue", tr lang "Blue" ), ( "heart-green", tr lang "Green" ), ( "heart-yellow", tr lang "Yellow" ), ( "heart-orange", tr lang "Orange" ), ( "heart-gray", tr lang "Gray" ), ( "heart-black", tr lang "Black" ) ]
                , div [ class "field soft-field" ]
                    [ label [] [ text (tr lang "We got together") ]
                    , input [ type_ "date", value relationshipDate, onInput SetRelationshipDate ] []
                    ]
                ]
            , button [ class "btn-primary", onClick SubmitRelationship ] [ text (tr lang "Save heart") ]
            ]
        ]



-- | Avatar display card with name, birthday, and edit button


viewAvatarCard : Language -> AvatarSide -> AvatarProfile -> Html Msg
viewAvatarCard lang side avatar =
    div [ class "avatar-card avatar-display-card" ]
        [ viewAvatarVisual avatar
        , h3 [ class "avatar-name" ] [ viewAvatarName avatar.name ]
        , viewAvatarBirthday avatar.birthday
        , button [ class "avatar-edit-button", type_ "button", onClick (OpenAvatarEditor side), title (tr lang "Edit avatar") ] [ text "✎" ]
        ]



-- | Partner display name, or an empty dot when the name is blank


viewAvatarName : String -> Html msg
viewAvatarName name =
    if String.isEmpty (String.trim name) then
        span [ class "avatar-name-dot", attribute "aria-label" "Name not set" ] []

    else
        text (String.trim name)



-- | Birthday date with a zodiac symbol, or a dash if not set


viewAvatarBirthday : String -> Html Msg
viewAvatarBirthday birthday =
    div [ class "avatar-birthday" ]
        (if String.isEmpty (String.trim birthday) then
            [ strong [] [ text "—" ] ]

         else
            [ strong [] [ text (formatDate birthday) ]
            , span [ class "zodiac-icon", title (zodiacName birthday) ] [ text (zodiacSymbol birthday) ]
            ]
        )



-- | Photo or CSS figure avatar, depending on the display mode setting


viewAvatarVisual : AvatarProfile -> Html Msg
viewAvatarVisual avatar =
    if avatar.mode == "photo" && not (String.isEmpty (String.trim avatar.photo)) then
        img [ class "avatar-photo", src (photoUrl avatar.photo), alt "Profile photo" ] []

    else
        viewAvatarFigure avatar



-- | CSS figure avatar assembled from color, accessory, and expression classes


viewAvatarFigure : AvatarProfile -> Html Msg
viewAvatarFigure avatar =
    div
        [ class ("avatar-figure " ++ avatar.figureColor ++ " accessory-" ++ avatar.accessory ++ " expression-" ++ avatar.expression) ]
        [ div [ class "avatar-head" ]
            [ div [ class "avatar-accessory" ] []
            , div [ class "avatar-eye left" ] []
            , div [ class "avatar-eye right" ] []
            , div [ class "avatar-mouth" ] []
            ]
        , div [ class "avatar-body" ] []
        , div [ class "avatar-arm left" ] []
        , div [ class "avatar-arm right" ] []
        ]



-- | Modal for editing a partner's avatar


viewAvatarEditor : Language -> AvatarSide -> AvatarProfile -> Html Msg
viewAvatarEditor lang side avatar =
    div [ class "modal-backdrop" ]
        [ div [ class "avatar-editor-modal" ]
            [ div [ class "form-header" ]
                [ h2 [] [ text (tr lang "Edit avatar") ]
                , button [ class "btn-icon", onClick CloseAvatarEditor ] [ text "✕" ]
                ]
            , div [ class "avatar-editor-preview" ]
                [ viewAvatarVisual avatar
                , viewAvatarBirthday avatar.birthday
                ]
            , viewAvatarControls lang side avatar
            ]
        ]



-- | Avatar editor form fields: name, display mode, colors, accessory, expression, and birthday


viewAvatarControls : Language -> AvatarSide -> AvatarProfile -> Html Msg
viewAvatarControls lang side avatar =
    div [ class "avatar-controls" ]
        [ div [ class "field required-field" ]
            [ label [] [ text (tr lang "Name") ]
            , input [ type_ "text", value avatar.name, onInput (SetAvatarName side), placeholder (tr lang "Name") ] []
            ]
        , avatarSelect (tr lang "Display") avatar.mode (SetAvatarMode side) [ ( "figure", tr lang "Figure" ), ( "photo", tr lang "Photo" ) ]
        , if avatar.mode == "photo" then
            div [ class "field soft-field" ]
                [ label [] [ text (tr lang "Photo") ]
                , div [ class "photo-name-row" ]
                    [ input [ type_ "text", value avatar.photo, onInput (SetAvatarPhoto side), placeholder "soubor-z-photos.jpg" ] []
                    , button [ class "btn-ghost", type_ "button", onClick (PickAvatarPhoto side) ] [ text (tr lang "Upload") ]
                    ]
                ]

          else
            div []
                [ div [ class "form-row" ]
                    [ avatarSelect (tr lang "Figure color")
                        avatar.figureColor
                        (SetAvatarFigureColor side)
                        [ ( "figure-black", tr lang "Black" ), ( "figure-red", tr lang "Red" ), ( "figure-blue", tr lang "Blue" ), ( "figure-green", tr lang "Green" ), ( "figure-yellow", tr lang "Yellow" ), ( "figure-orange", tr lang "Orange" ), ( "figure-pink", tr lang "Pink" ), ( "figure-purple", tr lang "Purple" ), ( "figure-gray", tr lang "Gray" ) ]
                    , avatarSelect (tr lang "Accessory")
                        avatar.accessory
                        (SetAvatarAccessory side)
                        [ ( "none", tr lang "No accessory" ), ( "flower", tr lang "Flower" ), ( "hat", tr lang "Hat" ), ( "crown", tr lang "Crown" ), ( "bow", tr lang "Bow" ), ( "halo", tr lang "Halo" ), ( "party", tr lang "Party hat" ), ( "heart", tr lang "Heart" ) ]
                    , avatarSelect (tr lang "Expression")
                        avatar.expression
                        (SetAvatarExpression side)
                        [ ( "smile", tr lang "Smile" ), ( "laugh", tr lang "Laughing" ), ( "neutral", tr lang "Neutral" ), ( "love", tr lang "In love" ), ( "sad", tr lang "Sad" ), ( "angry", tr lang "Angry" ), ( "surprised", tr lang "Surprised" ) ]
                    ]
                ]
        , div [ class "field soft-field" ]
            [ label [] [ text (tr lang "Birthday") ]
            , input [ type_ "date", value avatar.birthday, onInput (SetAvatarBirthday side) ] []
            ]
        , button [ class "btn-ghost", type_ "button", onClick (SubmitAvatar side) ] [ text (tr lang "Save") ]
        ]



-- | Reusable labeled select dropdown used throughout the avatar editor


avatarSelect : String -> String -> (String -> Msg) -> List ( String, String ) -> Html Msg
avatarSelect labelText current toMsg options =
    div [ class "field soft-field" ]
        [ label [] [ text labelText ]
        , select [ value current, onInput toMsg ]
            (List.map
                (\( val, caption ) ->
                    option [ value val, selected (val == current) ] [ text caption ]
                )
                options
            )
        ]



-- UPCOMING EVENTS
-- | Combine important-day and relationship events, keep only the soonest


nearestUpcomingEvents : Today -> Status RelationshipData -> List ImportantDay -> List UpcomingEvent
nearestUpcomingEvents today relationship importantDays =
    let
        importantDayEvents =
            List.map importantDayUpcomingEvent importantDays

        relationshipEvents =
            relationshipUpcomingEvents today relationship
    in
    nearestEvents (importantDayEvents ++ relationshipEvents)



-- | Convert an ImportantDay to a generic UpcomingEvent


importantDayUpcomingEvent : ImportantDay -> UpcomingEvent
importantDayUpcomingEvent day =
    { title = day.title
    , date = Maybe.withDefault { year = 1970, month = 1, day = 1 } (dateParts day.nextOccurrence)
    , days = day.daysUntil
    , kind = day.kind
    }



-- | Extract anniversary and monthiversary as UpcomingEvents from backend relationship data


relationshipUpcomingEvents : Today -> Status RelationshipData -> List UpcomingEvent
relationshipUpcomingEvents _ relationship =
    case relationship of
        Loaded rel ->
            List.filterMap identity
                [ relationshipEventFromBackend "relationship-monthiversary" "monthiversary" rel.nextMonthiversary rel.monthiversaryNum rel.monthiversaryDays
                , relationshipEventFromBackend "relationship-anniversary" "anniversary" rel.nextAnniversary rel.anniversaryNumber rel.anniversaryDays
                ]

        _ ->
            []



-- | Build one relationship UpcomingEvent from optional backend fields, returns Nothing when the date is missing


relationshipEventFromBackend : String -> String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe UpcomingEvent
relationshipEventFromBackend marker fallbackTitle maybeDate maybeNumber maybeDays =
    Maybe.andThen dateParts maybeDate
        |> Maybe.map
            (\date ->
                { title =
                    case maybeNumber of
                        Just n ->
                            String.fromInt n ++ ". " ++ fallbackTitle

                        Nothing ->
                            fallbackTitle
                , date = date
                , days = Maybe.withDefault 0 maybeDays
                , kind = marker
                }
            )



-- | Keep only events tied with the minimum daysUntil value


nearestEvents : List UpcomingEvent -> List UpcomingEvent
nearestEvents events =
    case events of
        [] ->
            []

        first :: rest ->
            let
                nearestDays =
                    List.foldl (\event best -> Basics.min event.days best) first.days rest
            in
            List.filter (\event -> event.days == nearestDays) events



-- | Translate a birthday or anniversary event title to the current language


translateUpcomingEventTitle : Language -> UpcomingEvent -> String
translateUpcomingEventTitle lang event =
    if event.kind == "birthday" then
        birthdayEventName lang event.title

    else if event.kind == "relationship-anniversary" then
        translateOrdinalEventName lang "anniversary" event.title

    else if event.kind == "relationship-monthiversary" then
        translateOrdinalEventName lang "monthiversary" event.title

    else
        event.title
