module CalendarDays exposing (..)

import Date as Date
import List.Extra
import Time


publicHolidays =
    [ Date.fromCalendarDate 2021 Time.Nov 15
    , Date.fromCalendarDate 2021 Time.Nov 1
    , Date.fromCalendarDate 2021 Time.Nov 1
    ]


type alias CalendarDate =
    { date : Date.Date
    , isHoliday : Bool
    , isWeekend : Bool
    }


generateCalendarDays : Date.Date -> Date.Date -> List Date.Date -> List Time.Weekday -> List CalendarDate -> List CalendarDate
generateCalendarDays start end holidays weekendDays result =
    let
        startPlusOneDay =
            Date.add Date.Days 1 start

        endLoop =
            Date.diff Date.Days start end == 0
    in
    if endLoop then
        result

    else
        let
            newResult =
                result
                    ++ [ CalendarDate
                            start
                            (List.any (\date -> date == start) holidays)
                            (List.any ((==) (Date.weekday start)) weekendDays)
                       ]
        in
        generateCalendarDays startPlusOneDay end holidays weekendDays newResult


calendarDaysIn2021 =
    generateCalendarDays (Date.fromCalendarDate 2021 Time.Jan 1) (Date.fromCalendarDate 2022 Time.Jan 1) publicHolidays [ Time.Sat, Time.Sun ] []


minusOneDay : Date.Date -> Date.Date
minusOneDay =
    Date.add Date.Days -1


toCalDate : List Date.Date -> List Time.Weekday -> Date.Date -> CalendarDate
toCalDate ph weekends date_ =
    CalendarDate
        date_
        (List.any (\date -> date == date_) ph)
        (List.any ((==) (Date.weekday date_)) weekends)


weekends_ =
    [ Time.Sat, Time.Sun ]


isHoliday : List CalendarDate -> Date.Date -> Bool
isHoliday calendarDates date =
    case List.head (List.filter (\d -> d.date == date) calendarDates) of
        Nothing ->
            False

        Just d ->
            d.isHoliday || d.isWeekend


traverseBackwards : List CalendarDate -> List Time.Weekday -> Date.Date -> Int -> List CalendarDate
traverseBackwards calendarDates weekends fromDate days =
    let
        recTraverseBack : List CalendarDate -> Date.Date -> Int -> List CalendarDate
        recTraverseBack result pointDate days_ =
            let
                pointDateAsCalendarDate =
                    toCalDate publicHolidays weekends pointDate

                newResult =
                    result ++ [ pointDateAsCalendarDate ]

                prevDate =
                    Date.add Date.Days -1 pointDate

                prevDayIsHoliday =
                    isHoliday calendarDates prevDate
            in
            if prevDayIsHoliday then
                recTraverseBack newResult prevDate days_

            else
                case days_ of
                    0 ->
                        newResult

                    int ->
                        recTraverseBack newResult prevDate (int - 1)
    in
    recTraverseBack [] fromDate days


traverseForwards : List CalendarDate -> List Time.Weekday -> Date.Date -> Int -> List CalendarDate
traverseForwards calendarDates weekends fromDate days =
    let
        recTraverseForward : List CalendarDate -> Date.Date -> Int -> List CalendarDate
        recTraverseForward result pointDate days_ =
            let
                pointDateAsCalendarDate =
                    toCalDate publicHolidays weekends pointDate

                newResult =
                    result ++ [ pointDateAsCalendarDate ]

                nextDate =
                    Date.add Date.Days 1 pointDate

                nextDayIsHoliday =
                    isHoliday calendarDates nextDate
            in
            if nextDayIsHoliday then
                recTraverseForward newResult nextDate days_

            else
                case days_ of
                    0 ->
                        newResult

                    int ->
                        recTraverseForward newResult nextDate (int - 1)
    in
    recTraverseForward [] fromDate days


getLeaveRangesFromDate : List CalendarDate -> List Time.Weekday -> Date.Date -> Int -> List (List CalendarDate)
getLeaveRangesFromDate calendarDates weekend_ date int =
    let
        sortedUnique : ( Int, Int ) -> List CalendarDate
        sortedUnique ( b, f ) =
            traverseBackwards calendarDates weekend_ date b
                ++ traverseForwards calendarDates weekend_ date f
                |> List.Extra.unique
                |> List.sortWith (\a b_ -> Date.compare a.date b_.date)

        recGetLeaveRangeFromDate : List (List CalendarDate) -> Date.Date -> ( Int, Int ) -> List (List CalendarDate)
        recGetLeaveRangeFromDate result date_ ( backwards, forwards ) =
            if backwards < 0 then
                result

            else
                result ++ (sortedUnique ( backwards, forwards ) :: recGetLeaveRangeFromDate result date_ ( backwards - 1, forwards + 1 ))
    in
    recGetLeaveRangeFromDate [] date ( int, 0 )


log dateInNov int =
    traverseBackwards calendarDaysIn2021 weekends_ (Date.fromCalendarDate 2021 Time.Nov dateInNov) int


log2 dateInNov int =
    traverseForwards calendarDaysIn2021 weekends_ (Date.fromCalendarDate 2021 Time.Nov dateInNov) int


mapToStrings =
    List.map (\d -> Date.toIsoString d.date)


runTests _ =
    let
        -- mapToStrings =
        --     List.map (\d -> Date.toIsoString d.date)
        test1 =
            (log 10 0 |> mapToStrings) == [ "2021-11-10" ]

        test2 =
            (log 10 1 |> mapToStrings) == [ "2021-11-10", "2021-11-09" ]

        test3 =
            (log 10 2 |> mapToStrings) == [ "2021-11-10", "2021-11-09", "2021-11-08", "2021-11-07", "2021-11-06" ]

        test4 =
            (log 12 3 |> mapToStrings) == [ "2021-11-12", "2021-11-11", "2021-11-10", "2021-11-09" ]

        test5 =
            (log2 12 0 |> mapToStrings) == [ "2021-11-12", "2021-11-13", "2021-11-14" ]

        test6 =
            (log2 11 0 |> mapToStrings) == [ "2021-11-11", "2021-11-12", "2021-11-13", "2021-11-14" ]

        test7 =
            (log2 11 1 |> mapToStrings) == [ "2021-11-11", "2021-11-12", "2021-11-13", "2021-11-14", "2021-11-15" ]

        test8 =
            (log2 12 1 |> mapToStrings) == [ "2021-11-12", "2021-11-13", "2021-11-14", "2021-11-15" ]
    in
    List.foldl (&&) True [ test1, test2, test3, test4, test5, test6, test7, test8 ]


test =
    getLeaveRangesFromDate calendarDaysIn2021 weekends_ (Date.fromCalendarDate 2021 Time.Nov 10) 2



-- filterMaxLength : List (List CalendarDate) -> List CalendarDate
-- filterMaxLength list =
--     List.map getItemWithMaxLength list


getItemWithMaxLength : List (List a) -> List a
getItemWithMaxLength list =
    Maybe.withDefault []
        (List.Extra.maximumWith
            (\a b ->
                if List.length a >= List.length b then
                    GT

                else
                    LT
            )
            list
        )
