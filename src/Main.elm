module Main exposing (..)

import Browser
import CalendarDays as CD
import CalendarGenerator as C
import Date
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
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
    }


init : { year : Int } -> ( Model, Cmd Msg )
init { year } =
    ( Model
        (String.fromInt year)
        []
        CD.weekends_
        []
        2
        True
    , Task.perform (\_ -> GenerateLongWeekends) (Task.succeed 1)
    )


type Msg
    = NoOp
    | GenerateLongWeekends
    | UpdateForcedLeaves Int
    | UpdateYear String
    | ToggleNonWeekends Bool
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
            update GenerateLongWeekends
                { model
                    | year =
                        String.trim str
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
                            update GenerateLongWeekends
                                { model
                                    | publicHolidays = newPhList
                                }


view : Model -> H.Html Msg
view model =
    H.div
        [ Attr.style "display" "grid"
        , Attr.style "grid-auto-flow" "column"
        , Attr.style "grid-template-columns" "repeat(2, auto)"
        , Attr.style "grid-gap" "5rem"
        , Attr.class "p-10"
        ]
        [ H.div
            []
            [ H.div [ Attr.class "flex flex-col gap-10" ]
                [ H.div []
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
                ]
            ]
        , H.div
            [ Attr.class "p-4"
            ]
            [ viewYear (model.year |> String.toInt |> Maybe.withDefault 2021) model.publicHolidays (List.concat model.longWeekends) ]
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
        [ Attr.class "calendar"
        , Attr.style "display" "grid"
        , Attr.style "grid-auto-flow" "row"
        , Attr.style "grid-template-columns" "repeat(4, 1fr)"
        , Attr.style "grid-template-rows" "repeat(3, auto)"
        , Attr.style "grid-gap" "3rem"
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
