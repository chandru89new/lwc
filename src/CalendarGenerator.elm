module CalendarGenerator exposing (..)

import Date
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra
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
            List.Extra.groupsOf 7 datesAsListofMaybes
    in
    result


viewMonth : (Msg -> msg) -> List PublicHoliday -> List HighlightDate -> Time.Month -> Time.Weekday -> Int -> H.Html msg
viewMonth toMsg phs lws month startOfWeek year =
    let
        listOfWeeks =
            generateMonthList month year startOfWeek
    in
    H.map toMsg
        (H.div
            [ Attr.class "mb-5"
            ]
            ([ viewMonthHeader month year
             , viewWeekHeader startOfWeek
             ]
                ++ List.map (viewWeek phs lws) listOfWeeks
            )
        )


viewWeek : List PublicHoliday -> List HighlightDate -> List (Maybe Date.Date) -> H.Html Msg
viewWeek phs lws week =
    H.div weekDivStyle
        (List.map
            (viewDate
                phs
                lws
            )
            week
        )


viewDate : List PublicHoliday -> List HighlightDate -> Maybe Date.Date -> H.Html Msg
viewDate phs lwds maybeDate =
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
    in
    H.span
        (dateBoxStyle
            ++ [ Attr.class "date border border-transparent"
               , Attr.class
                    (if isPh then
                        "border border-green-500"

                     else
                        ""
                    )
               , Attr.class
                    (if isLwDate then
                        "text-green-800 bg-green-100"

                     else
                        ""
                    )
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
            (List.map Date.numberToWeekday weekdayNumberRange)
        )


weekDivStyle =
    [ Attr.class "week"
    , Attr.style "display" "grid"
    , Attr.style "grid-auto-flow" "column"
    , Attr.style "grid-template-columns" "repeat(7, 1fr)"
    ]


dateBoxStyle =
    [ Attr.class "date-box inline-flex items-center justify-center text-sm p-1"

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
