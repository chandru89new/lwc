port module Main exposing (..)

import Browser
import CalendarDays as CD
import CalendarGenerator as C
import Date
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Encode as Encoder
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { year : String
    , publicHolidays : List Date.Date
    , weekendDays : List Time.Weekday
    , longWeekends : List (List Date.Date)
    , numberOfForcedLeaves : Int
    , showNonWeekendsToo : Bool
    , startOfWeek : Time.Weekday
    }


init : { year : Int, initPublicHolidays : List String } -> ( Model, Cmd Msg )
init { year, initPublicHolidays } =
    ( Model
        (String.fromInt year)
        (List.map (changeYear year)
            (List.filterMap
                (Date.fromIsoString >> Result.toMaybe)
                initPublicHolidays
            )
        )
        CD.weekends_
        []
        2
        True
        Time.Sun
    , Task.perform (\_ -> GenerateLongWeekends) (Task.succeed 1)
    )


type Msg
    = NoOp
    | GenerateLongWeekends
    | UpdateForcedLeaves Int
    | UpdateYear String
    | SaveHolidays
    | ToggleNonWeekends Bool
    | ToggleWeekendDays Time.Weekday Bool
    | UpdateStartOfWeek Time.Weekday
    | CMsg C.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateForcedLeaves int ->
            update GenerateLongWeekends
                { model
                    | numberOfForcedLeaves =
                        if int < 0 then
                            0

                        else
                            int
                }

        UpdateYear str ->
            let
                year =
                    str |> (String.toInt >> Maybe.withDefault 2021)

                newPh =
                    List.map (changeYear year) model.publicHolidays
            in
            update GenerateLongWeekends
                { model
                    | year =
                        String.trim str
                    , publicHolidays = newPh
                }

        ToggleNonWeekends bool ->
            update GenerateLongWeekends { model | showNonWeekendsToo = bool }

        GenerateLongWeekends ->
            let
                longWeekendsList =
                    List.map (\date -> CD.getLeaveRangesFromDate model.publicHolidays model.weekendDays date model.numberOfForcedLeaves |> CD.getItemWithMaxLength) model.publicHolidays

                lwdFilteredForWeekends =
                    if model.showNonWeekendsToo then
                        longWeekendsList

                    else
                        List.filter hasWeekendsInIt longWeekendsList

                hasWeekendsInIt : List Date.Date -> Bool
                hasWeekendsInIt list =
                    List.any (\date -> List.any (\wd -> wd == Date.weekday date) model.weekendDays) list
            in
            ( { model | longWeekends = lwdFilteredForWeekends }, Cmd.none )

        SaveHolidays ->
            ( model
            , Cmd.batch
                [ saveToStorage (Encoder.encode 0 (Encoder.list Encoder.string (List.map Date.toIsoString model.publicHolidays)))
                , Task.perform (\_ -> GenerateLongWeekends) (Task.succeed Nothing)
                ]
            )

        ToggleWeekendDays weekday checked ->
            let
                newWeekendDays =
                    if checked then
                        weekday :: model.weekendDays

                    else
                        List.filter ((/=) weekday) model.weekendDays
            in
            update GenerateLongWeekends { model | weekendDays = newWeekendDays }

        UpdateStartOfWeek startOfWeek ->
            ( { model | startOfWeek = startOfWeek }, Cmd.none )

        CMsg cmsg ->
            case cmsg of
                C.ClickedDate date ->
                    case date of
                        Nothing ->
                            ( model, Cmd.none )

                        Just d ->
                            let
                                newPhList =
                                    if List.any ((==) d) model.publicHolidays then
                                        List.filter ((/=) d) model.publicHolidays

                                    else
                                        d :: model.publicHolidays
                            in
                            update SaveHolidays
                                { model
                                    | publicHolidays = newPhList
                                }


view : Model -> H.Html Msg
view model =
    H.div
        [ Attr.style "display" "grid"
        , Attr.style "grid-auto-flow" "column"
        , Attr.style "grid-template-columns" "1fr 3fr"
        , Attr.style "grid-gap" "5rem"
        ]
        [ H.div
            [ Attr.class "p-10" ]
            [ H.div [ Attr.class "flex flex-col gap-10" ]
                [ H.h1
                    [ Attr.class "text-xl text-green-600 border-b pb-2 border-green-200"
                    ]
                    [ H.text "Long-weekend calculator" ]
                , H.div []
                    [ H.div
                        []
                        [ H.text "1. Click on a date to mark it as a public holiday"
                        ]
                    ]
                , H.div []
                    [ H.div [ Attr.class "mb-1" ] [ H.text "2. Select year" ]
                    , H.input
                        [ Attr.class "border p-1 rounded"
                        , Ev.onInput UpdateYear
                        , Attr.type_ "number"
                        , Attr.value
                            model.year
                        ]
                        []
                    ]
                , H.div []
                    [ H.div
                        [ Attr.class "mb-1"
                        ]
                        [ H.text "3. Number of time-offs you are willing to take*" ]
                    , H.input
                        [ Attr.type_ "number"
                        , Attr.class "border p-1 rounded"
                        , Attr.value
                            (String.fromInt model.numberOfForcedLeaves)
                        , Ev.onInput (String.toInt >> Maybe.withDefault 0 >> UpdateForcedLeaves)
                        ]
                        []
                    , H.div [ Attr.class "text-sm mt-2" ]
                        [ H.text "* For eg, if a public holiday falls on Thursday, you might be willing to take the Friday off just to get a longer weekend! In this case, you are willing to take 1 time-off and you would put 1 as the value." ]
                    ]
                , H.div []
                    [ H.input
                        [ Attr.id "non-weekends-toggle"
                        , Attr.type_ "checkbox"
                        , Attr.class "cursor-pointer"
                        , Attr.checked model.showNonWeekendsToo
                        , Ev.onCheck ToggleNonWeekends
                        ]
                        []
                    , H.label
                        [ Attr.class "cursor-pointer ml-1"
                        , Attr.for "non-weekends-toggle"
                        ]
                        [ H.text "Show non-weekend possibilities too"
                        ]
                    ]
                , H.div []
                    [ H.div []
                        [ H.text "Weekends" ]
                    , viewWeekendSelector model.weekendDays
                    ]
                , H.div []
                    [ H.div [] [ H.text "Week starts on" ]
                    , viewWeekStartSelector model.startOfWeek
                    ]
                , H.div [] [ viewLegend ]
                , H.div []
                    [ H.a [ Attr.href "/about.html" ] [ H.text "About" ] ]
                ]
            ]
        , H.div
            [ Attr.class "p-10 border-l"
            ]
            [ viewYear (model.year |> String.toInt |> Maybe.withDefault 2021) model.publicHolidays (List.concat model.longWeekends) model.startOfWeek ]
        ]


viewPotentialWeekend : List CD.CalendarDate -> H.Html Msg
viewPotentialWeekend list =
    H.div []
        [ H.text <| String.join ", " (List.map toString list)
        ]


toString : CD.CalendarDate -> String
toString a =
    Date.toIsoString a.date


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewYear : Int -> List C.PublicHoliday -> List C.HighlightDate -> Time.Weekday -> H.Html Msg
viewYear year phs lws startOfWeek =
    H.div
        [ Attr.class "calendar"
        , Attr.style "display" "grid"
        , Attr.style "grid-auto-flow" "row"
        , Attr.style "grid-template-columns" "repeat(4, 1fr)"
        , Attr.style "grid-template-rows" "repeat(3, auto)"
        , Attr.style "grid-gap" "3rem"
        ]
        (List.map
            (\month -> C.viewMonth phs lws month startOfWeek year |> H.map CMsg)
            months
        )


months =
    [ Time.Jan
    , Time.Feb
    , Time.Mar
    , Time.Apr
    , Time.May
    , Time.Jun
    , Time.Jul
    , Time.Aug
    , Time.Sep
    , Time.Oct
    , Time.Nov
    , Time.Dec
    ]


viewLegend : H.Html Msg
viewLegend =
    H.div []
        [ H.div [ Attr.class "text-sm mb-3" ] [ H.text "Legend" ]
        , H.div [ Attr.class "flex items-center" ]
            [ H.span
                (List.map (\( a, b ) -> Attr.style a b) [ ( "width", "16px" ), ( "height", "16px" ) ]
                    ++ [ Attr.class "bg-green-200 inline-block"
                       ]
                )
                []
            , H.span [ Attr.class "ml-2 text-xs" ] [ H.text "Potential long leave" ]
            ]
        , H.div [ Attr.class "mt-2 flex items-center" ]
            [ H.span
                (List.map (\( a, b ) -> Attr.style a b) [ ( "width", "16px" ), ( "height", "16px" ) ]
                    ++ [ Attr.class "border border-green-500 inline-block"
                       ]
                )
                []
            , H.span [ Attr.class "ml-2 text-xs" ] [ H.text "Day marked as public holiday" ]
            ]
        ]


port saveToStorage : String -> Cmd msg


changeYear : Int -> Date.Date -> Date.Date
changeYear year date =
    Date.fromCalendarDate year (Date.month date) (Date.day date)


weekdays =
    [ Time.Mon
    , Time.Tue
    , Time.Wed
    , Time.Thu
    , Time.Fri
    , Time.Sat
    , Time.Sun
    ]


viewWeekendSelector : List Time.Weekday -> H.Html Msg
viewWeekendSelector weekends =
    let
        renderCheckbox : List Time.Weekday -> Time.Weekday -> H.Html Msg
        renderCheckbox selectedWeekends weekday =
            H.span [ Attr.class "inline-block mr-3" ]
                [ H.input
                    [ Attr.type_ "checkbox"
                    , Attr.id ("checkbox-" ++ C.weekdayToSingleCharacter weekday)
                    , Attr.checked (List.member weekday selectedWeekends)
                    , Ev.onCheck (ToggleWeekendDays weekday)
                    ]
                    []
                , H.label
                    [ Attr.class "ml-1"
                    , Attr.for ("checkbox-" ++ C.weekdayToSingleCharacter weekday)
                    ]
                    [ H.text <| C.weekdayToSingleCharacter weekday ]
                ]
    in
    H.div []
        (List.map
            (renderCheckbox weekends)
            weekdays
        )


viewWeekStartSelector : Time.Weekday -> H.Html Msg
viewWeekStartSelector selectedWeekday =
    let
        option weekday =
            H.option
                [ Attr.selected (weekday == selectedWeekday)
                , Attr.value (C.weekdayToTripleCharacterString weekday)
                , Ev.onClick <| UpdateStartOfWeek weekday
                ]
                [ H.text (C.weekdayToTripleCharacterString weekday) ]
    in
    H.div []
        [ H.select []
            (List.map option weekdays)
        ]
