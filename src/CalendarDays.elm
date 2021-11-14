module CalendarDays exposing (..)

import Date
import List.Extra as List
import Time


isHoliday : List Date.Date -> List Time.Weekday -> Date.Date -> Bool
isHoliday phs weekends date =
    List.any ((==) date) phs || List.any ((==) (Date.weekday date)) weekends


traverseBackwards : List Date.Date -> List Time.Weekday -> Date.Date -> Int -> List Date.Date
traverseBackwards publicHolidays weekends fromDate days =
    let
        recTraverseBack : List Date.Date -> Date.Date -> Int -> List Date.Date
        recTraverseBack result pointDate days_ =
            let
                newResult =
                    result ++ [ pointDate ]

                prevDate =
                    Date.add Date.Days -1 pointDate

                prevDayIsHoliday =
                    isHoliday publicHolidays weekends prevDate
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


traverseForwards : List Date.Date -> List Time.Weekday -> Date.Date -> Int -> List Date.Date
traverseForwards publicHolidays weekends fromDate days =
    let
        recTraverseForward : List Date.Date -> Date.Date -> Int -> List Date.Date
        recTraverseForward result pointDate days_ =
            let
                newResult =
                    result ++ [ pointDate ]

                nextDate =
                    Date.add Date.Days 1 pointDate

                nextDayIsHoliday =
                    isHoliday publicHolidays weekends nextDate
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


getLeaveRangesFromDate : List Date.Date -> List Time.Weekday -> Date.Date -> Int -> List (List Date.Date)
getLeaveRangesFromDate publicHolidays weekend_ date int =
    let
        sortedUnique : ( Int, Int ) -> List Date.Date
        sortedUnique ( b, f ) =
            traverseBackwards publicHolidays weekend_ date b
                ++ traverseForwards publicHolidays weekend_ date f
                |> List.unique
                |> List.sortWith Date.compare

        recGetLeaveRangeFromDate : List (List Date.Date) -> Date.Date -> ( Int, Int ) -> List (List Date.Date)
        recGetLeaveRangeFromDate result date_ ( backwards, forwards ) =
            if backwards < 0 then
                result

            else
                result ++ (sortedUnique ( backwards, forwards ) :: recGetLeaveRangeFromDate result date_ ( backwards - 1, forwards + 1 ))
    in
    recGetLeaveRangeFromDate [] date ( int, 0 )


getItemWithMaxLength : List (List a) -> List a
getItemWithMaxLength =
    Maybe.withDefault []
        << List.maximumWith
            (\a b ->
                if List.length a >= List.length b then
                    GT

                else
                    LT
            )


weekends_ : List Time.Weekday
weekends_ =
    [ Time.Sun, Time.Sat ]
