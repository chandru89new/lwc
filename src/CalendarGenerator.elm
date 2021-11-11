module CalendarGenerator exposing (..)

import Date
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra
import Time


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
            List.Extra.groupsOf 7 datesAsListofMaybes
    in
    result


viewMonth : (Msg -> msg) -> Time.Month -> Int -> Time.Weekday -> H.Html msg
viewMonth toMsg month year startOfWeek =
    let
        listOfWeeks =
            generateMonthList month year startOfWeek
    in
    H.map toMsg
        (H.div
            [ Attr.style "width" "calc(32px * 7)"
            , Attr.class "mb-5"
            ]
            ([ viewMonthHeader month year
             , viewWeekHeader
             ]
                ++ List.map viewWeek listOfWeeks
            )
        )


viewWeek : List (Maybe Date.Date) -> H.Html Msg
viewWeek week =
    H.div []
        (List.map
            viewDate
            week
        )


viewDate : Maybe Date.Date -> H.Html Msg
viewDate maybeDate =
    let
        dateAsString =
            case maybeDate of
                Nothing ->
                    "-"

                Just d ->
                    String.fromInt <| Date.day d
    in
    H.span
        (dateBoxStyle ++ [ Ev.onClick (ClickedDate maybeDate) ])
        [ H.text dateAsString ]


viewWeekHeader : H.Html a
viewWeekHeader =
    let
        span text =
            H.span
                (dateBoxStyle
                    ++ [ Attr.class "text-gray-300"
                       ]
                )
                [ H.text text ]

        toSpan weekday =
            case weekday of
                Time.Mon ->
                    span "M"

                Time.Tue ->
                    span "T"

                Time.Wed ->
                    span "W"

                Time.Thu ->
                    span "T"

                Time.Fri ->
                    span "F"

                Time.Sat ->
                    span "S"

                Time.Sun ->
                    span "S"
    in
    H.div weekDivStyle
        (List.map
            toSpan
            [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri, Time.Sat, Time.Sun ]
        )


weekDivStyle =
    [ Attr.class "week" ]


dateBoxStyle =
    [ Attr.class "date-box inline-flex items-center justify-center font-mono text-sm"
    , Attr.style "height" "32px"
    , Attr.style "width" "32px"
    , Attr.style "font-" "monospace"
    ]


viewMonthHeader : Time.Month -> Year -> H.Html a
viewMonthHeader month year =
    H.div
        [ Attr.class "text-center font-mono text-sm uppercase mb-3"
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
