module CalendarGenerator exposing (..)

import Date
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra as List
import Time


type alias PublicHoliday =
    Date.Date


type Msg
    = ClickedDate (Maybe Date.Date)


getEndOfWeek : Time.Weekday -> Time.Weekday
getEndOfWeek startOfWeek =
    case startOfWeek of
        Time.Sun ->
            Time.Sat

        Time.Mon ->
            Time.Sun

        Time.Tue ->
            Time.Mon

        Time.Wed ->
            Time.Tue

        Time.Thu ->
            Time.Wed

        Time.Fri ->
            Time.Thu

        Time.Sat ->
            Time.Fri


timeWeekdayToDateInterval : Time.Weekday -> Date.Interval
timeWeekdayToDateInterval weekday =
    case weekday of
        Time.Sun ->
            Date.Sunday

        Time.Mon ->
            Date.Monday

        Time.Tue ->
            Date.Tuesday

        Time.Wed ->
            Date.Wednesday

        Time.Thu ->
            Date.Thursday

        Time.Fri ->
            Date.Friday

        Time.Sat ->
            Date.Saturday


generateMonthList : Time.Month -> Int -> Time.Weekday -> List (List (Maybe Date.Date))
generateMonthList month year startOfWeek =
    let
        endOfWeek =
            getEndOfWeek startOfWeek

        startDateOfMonth =
            Date.fromCalendarDate year month 1

        endDateOfMonth =
            Date.add Date.Days -1 (Date.add Date.Months 1 startDateOfMonth)

        padLeftAmount =
            Date.diff Date.Days (Date.floor (timeWeekdayToDateInterval startOfWeek) startDateOfMonth) startDateOfMonth

        padRightAmount =
            Date.diff Date.Days endDateOfMonth (Date.ceiling (timeWeekdayToDateInterval endOfWeek) endDateOfMonth)

        datesAsListofMaybes =
            List.repeat padLeftAmount Nothing ++ List.map Just (Date.range Date.Day 1 startDateOfMonth (Date.add Date.Days 1 endDateOfMonth)) ++ List.repeat padRightAmount Nothing

        result =
            List.groupsOf 7 datesAsListofMaybes
    in
    result


viewMonth : DaysData -> Time.Month -> Time.Weekday -> Int -> H.Html Msg
viewMonth daysData month startOfWeek year =
    let
        listOfWeeks =
            generateMonthList month year startOfWeek
    in
    H.div
        [ Attr.class ""
        ]
        ([ viewMonthHeader month year
         , viewWeekHeader startOfWeek
         ]
            ++ List.map (viewWeek daysData) listOfWeeks
        )


viewWeek : DaysData -> List (Maybe Date.Date) -> H.Html Msg
viewWeek daysData week =
    H.div weekDivStyle
        (List.map
            (viewDate
                daysData
            )
            week
        )


type alias DaysData =
    { phs : List PublicHoliday
    , lwds : List HighlightDate
    , weekends : List Time.Weekday
    }


viewDate : DaysData -> Maybe Date.Date -> H.Html Msg
viewDate { phs, lwds, weekends } maybeDate =
    let
        dateAsString =
            case maybeDate of
                Nothing ->
                    " "

                Just d ->
                    String.fromInt <| Date.day d

        isPh =
            Maybe.withDefault False (Maybe.map (\date -> List.any ((==) date) phs) maybeDate)

        isLwDate =
            Maybe.withDefault False <| Maybe.map (\date -> List.any ((==) date) lwds) maybeDate

        isWeekend =
            Maybe.map Date.weekday maybeDate
                |> Maybe.map (\weekday -> List.any ((==) weekday) weekends)
                |> Maybe.withDefault False

        isTimeOff =
            isLwDate && not isPh && not isWeekend

        className =
            case ( isPh, isLwDate, isTimeOff ) of
                ( True, _, _ ) ->
                    "border border-green-500 bg-green-100"

                ( _, _, True ) ->
                    "bg-red-100"

                ( _, True, _ ) ->
                    "text-green-800 bg-green-100"

                _ ->
                    ""
    in
    H.span
        (dateBoxStyle
            ++ [ Attr.class "date border border-transparent cursor-pointer"
               , Attr.class
                    className
               , Ev.onClick
                    (ClickedDate maybeDate)
               ]
        )
        [ H.text dateAsString ]


viewWeekHeader : Time.Weekday -> H.Html a
viewWeekHeader startOfWeek =
    let
        startOfWeekAsNumber =
            Date.weekdayToNumber startOfWeek

        weekdayNumberRange =
            List.range (startOfWeekAsNumber - 1) (startOfWeekAsNumber + 5)
                |> List.map (modBy 7 >> (+) 1)

        span text =
            H.span
                (dateBoxStyle
                    ++ [ Attr.class "text-gray-300"
                       ]
                )
                [ H.text text ]

        toSpan =
            weekdayToSingleCharacter >> span
    in
    H.div weekDivStyle
        (List.map
            toSpan
            (List.map Date.numberToWeekday weekdayNumberRange)
        )


weekDivStyle =
    [ Attr.class "week border-b"
    , Attr.style "display" "grid"
    , Attr.style "grid-auto-flow" "column"
    , Attr.style "grid-template-columns" "repeat(7, 1fr)"
    ]


dateBoxStyle =
    [ Attr.class "date-box inline-flex items-center justify-center text-sm p-1 "

    -- , Attr.style "height" "32px"
    ]


viewMonthHeader : Time.Month -> Year -> H.Html a
viewMonthHeader month year =
    H.div
        [ Attr.class "text-center text-sm uppercase mb-3"
        ]
        [ H.text (monthToString month ++ " " ++ String.fromInt year) ]


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


type alias Year =
    Int


type alias HighlightDate =
    Date.Date


weekdayToSingleCharacter : Time.Weekday -> String
weekdayToSingleCharacter weekday =
    case weekday of
        Time.Mon ->
            "M"

        Time.Tue ->
            "T"

        Time.Wed ->
            "W"

        Time.Thu ->
            "T"

        Time.Fri ->
            "F"

        Time.Sat ->
            "S"

        Time.Sun ->
            "S"


weekdayToTripleCharacterString : Time.Weekday -> String
weekdayToTripleCharacterString weekday =
    case weekday of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


stringToWeekday : String -> Time.Weekday
stringToWeekday weekday =
    case weekday of
        "Mon" ->
            Time.Mon

        "Tue" ->
            Time.Tue

        "Wed" ->
            Time.Wed

        "Thu" ->
            Time.Thu

        "Fri" ->
            Time.Fri

        "Sat" ->
            Time.Sat

        "Sun" ->
            Time.Sun

        _ ->
            Time.Sun
