module Main exposing (..)

import Browser
import CalendarDays as CD
import CalendarGenerator as C
import Date
import Html as H
import Html.Attributes as Attr
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
    { calendarDays : List CD.CalendarDate
    , publicHolidays : List Date.Date
    , weekendDays : List Time.Weekday
    , longWeekends : List (List CD.CalendarDate)
    , numberOfForcedLeaves : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        year =
            2021

        startDate =
            Date.fromCalendarDate year Time.Jan 1

        endDate =
            Date.fromCalendarDate (year + 1) Time.Jan 1
    in
    ( Model
        (CD.generateCalendarDays startDate endDate [] CD.weekends_ [])
        [ Date.fromCalendarDate 2021 Time.Jan 26
        , Date.fromCalendarDate 2021 Time.Apr 14
        , Date.fromCalendarDate 2021 Time.Aug 15
        , Date.fromCalendarDate 2021 Time.Oct 2
        , Date.fromCalendarDate 2021 Time.Nov 4
        , Date.fromCalendarDate 2021 Time.Dec 25
        ]
        CD.weekends_
        []
        1
    , Task.perform (\_ -> GenerateLongWeekends) (Task.succeed 1)
    )


type Msg
    = NoOp
    | GenerateLongWeekends
    | CMsg C.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateLongWeekends ->
            let
                longWeekendsList =
                    List.map (\date -> CD.getLeaveRangesFromDate model.calendarDays model.publicHolidays model.weekendDays date model.numberOfForcedLeaves |> CD.getItemWithMaxLength) model.publicHolidays
            in
            ( { model | longWeekends = longWeekendsList }, Cmd.none )

        CMsg cmsg ->
            let
                _ =
                    Debug.log "cmsg" cmsg
            in
            ( model, Cmd.none )


view : Model -> H.Html Msg
view model =
    H.div []
        [ viewYear 2021 model.publicHolidays (List.concat model.longWeekends |> List.map .date)
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


viewYear : Int -> List C.PublicHoliday -> List C.HighlightDate -> H.Html Msg
viewYear year phs lws =
    H.div
        [ Attr.class "grid"
        ]
        (List.map
            (\month -> C.viewMonth CMsg phs lws month Time.Sun year)
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
