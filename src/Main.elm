module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Task
import Time exposing (Month(..), Weekday(..))


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ZonedTime =
    { zone : Time.Zone
    , time : Time.Posix
    }


type Model
    = Initializing
    | Loaded ZonedTime


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initializing
    , Task.perform identity (Task.map2 InitializeZonedTime Time.here Time.now)
    )


type Msg
    = Tick Time.Posix
    | InitializeZonedTime Time.Zone Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( case model of
                Initializing ->
                    model

                Loaded zonedTime ->
                    Loaded { zonedTime | time = newTime }
            , Cmd.none
            )

        InitializeZonedTime zone time ->
            ( case model of
                Initializing ->
                    Loaded { zone = zone, time = time }

                Loaded zonedTime ->
                    model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


toEnglishWeekday : Weekday -> String
toEnglishWeekday weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


toEnglishMonth : Month -> String
toEnglishMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "Feburary"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


toHumanReadableTime : Time.Zone -> Time.Posix -> String
toHumanReadableTime zone time =
    String.concat
        [ String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone time
        , ":"
        , String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone time
        , ":"
        , String.padLeft 2 '0' <| String.fromInt <| Time.toSecond zone time
        , " on "
        , toEnglishWeekday (Time.toWeekday zone time)
        , " "
        , toEnglishMonth (Time.toMonth zone time)
        , " "
        , String.fromInt (Time.toDay zone time)
        , ", "
        , String.fromInt (Time.toYear zone time)
        ]


black : Color
black =
    rgb255 10 10 10


white : Color
white =
    rgb255 225 225 225


blue : Color
blue =
    rgb255 21 101 192


viewInitializing : Browser.Document Msg
viewInitializing =
    { title = "Countdown!"
    , body =
        [ layout
            [ Background.color black
            , Font.family
                [ Font.external
                    { name = "Work Sans"
                    , url = "https://fonts.googleapis.com/css2?family=Work+Sans&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.color white
            , Font.center
            ]
          <|
            column [ width fill, height fill ] [ el [ width fill, height fill ] <| text "Loading..." ]
        ]
    }


viewLoaded : ZonedTime -> Browser.Document Msg
viewLoaded zonedTime =
    { title = "Countdown!"
    , body =
        [ layout
            [ Background.color black
            , Font.family
                [ Font.external
                    { name = "Work Sans"
                    , url = "https://fonts.googleapis.com/css2?family=Work+Sans&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.color white
            , Font.center
            , padding 50
            ]
          <|
            column [ width fill, height fill ]
                [ row [ width fill, height fill ]
                    [ el [ Background.color blue, width fill, height fill ] <|
                        el [ width fill, centerY ] <|
                            text <|
                                "It is now "
                                    ++ toHumanReadableTime zonedTime.zone zonedTime.time
                    ]
                ]
        ]
    }


view : Model -> Browser.Document Msg
view model =
    case model of
        Initializing ->
            viewInitializing

        Loaded zonedTime ->
            viewLoaded zonedTime
