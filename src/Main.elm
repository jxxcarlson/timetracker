module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import DateTime exposing (NaiveDateTime(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Log exposing (..)
import Style
import TestData exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)


type AppMode
    = Logging
    | Editing


type PhoneAppMode
    = ShowingLogs
    | ShowingEvents
    | ShowingChart


type TimerState
    = TSInitial
    | TSRunning
    | TSPaused


type TimerCommand
    = TCStart
    | TCPause
    | TCContinue
    | TCLog
    | TCReset


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    , logs : List Log
    , maybeCurrentLog : Maybe Log
    , maybeCurrentEvent : Maybe Event
    , logFilterString : String
    , appMode : AppMode
    , currentTime : Posix
    , dateFilter : DateFilter
    , timeZoneOffset : Int
    , filterState : EventGrouping
    }



--
-- MSG
--


type Msg
    = NoOp
    | GetEvents Int
    | SetCurrentEvent Event
    | SetGroupFilter EventGrouping


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , logs = [ TestData.log ]
      , maybeCurrentLog = Just TestData.log
      , maybeCurrentEvent = Just TestData.e1
      , logFilterString = ""
      , appMode = Logging
      , currentTime = Time.millisToPosix 0
      , dateFilter = NoDateFilter
      , timeZoneOffset = 5
      , filterState = NoGrouping
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none



--
-- UPDATE
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetEvents logId ->
            let
                maybeLog =
                    List.filter (\log -> log.id == logId) model.logs
                        |> List.head
            in
            ( { model | maybeCurrentEvent = Nothing, maybeCurrentLog = maybeLog }, Cmd.none )

        SetCurrentEvent event_ ->
            ( { model | maybeCurrentEvent = Just event_ }, Cmd.none )

        SetGroupFilter filterState ->
            ( { model | filterState = filterState }, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainView model)


mainView : Model -> Element Msg
mainView model =
    row (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ -- filterPanel model
          logListPanel model
        , eventListDisplay model
        ]



--         --     , eventListDisplay model
--         --     , case model.appMode of
--         --         Logging ->
--         --             eventPanel model
--         --
--         --         Editing ->
--         --             editPanel model
--         --     ]
--         -- , controlPanel model
--         ]
--
--


eventListDisplay : Model -> Element Msg
eventListDisplay model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLog model
        ]


viewLog : Model -> Element Msg
viewLog model =
    case model.maybeCurrentLog of
        Nothing ->
            column [ spacing 12, padding 20, height (px 500) ]
                [ el [ Font.size 16, Font.bold ] (text "No events available")
                ]

        Just currentLog ->
            let
                today =
                    DateTime.naiveDateStringFromPosix model.currentTime

                events2 =
                    Log.dateFilter today model.dateFilter currentLog.data

                eventSum_ =
                    Log.eventSum events2

                events : List Event
                events =
                    groupingFilter model.timeZoneOffset model.filterState events2

                nEvents =
                    List.length events |> toFloat

                average =
                    TypedTime.multiply (1.0 / nEvents) eventSum_
            in
            column [ spacing 12, padding 20, height (px 430), scrollbarY ]
                [ el [ Font.size 16, Font.bold ] (text (Maybe.map .name model.maybeCurrentLog |> Maybe.withDefault "XXX"))
                , indexedTable [ spacing 4, Font.size 12 ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text "idx")
                          , width = px (indexWidth model.appMode)
                          , view = indexButton model
                          }
                        , { header = el [ Font.bold ] (text "Date")
                          , width = px 80

                          --, view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.dateStringOfDateTimeString <| (\ndt -> DateTime.humanDateFromNaiveDateTime ndt) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Time")
                          , width = px 80
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.timeStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Value")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| TypedTime.timeAsStringWithUnit Minutes event.duration)
                          }
                        ]
                    }
                , row [ spacing 24, alignBottom, alignRight ]
                    [ el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Average: " ++ TypedTime.timeAsStringWithUnit Minutes average)
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Total: " ++ TypedTime.timeAsStringWithUnit Minutes eventSum_)
                    ]
                ]



-- editPanel model =
--     column [ spacing 12, width (px 300), alignTop ]
--         [ el [ Font.size 16, Font.bold, alignTop ] (text "Edit Panel")
--         , logEditPanel model
--         , logEventPanel model
--         ]
--
--
-- filterPanel model =
--     row [ spacing 8 ]
--         [ el [ Font.bold ] (text "Filter:")
--         , inputLogNameFilter model
--         , el [ Font.bold ] (text "Since:")
--         , inputEventDateFilter model
--         , row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, logModeButton model ]
--         ]


logListPanel : Model -> Element Msg
logListPanel model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLogs model
        ]



--
-- eventListDisplay : Model -> Element Msg
-- eventListDisplay model =
--     column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
--         [ viewEvents model
--         ]
--
--
-- controlPanel model =
--     column [ padding 8, Border.width 1, width (px 562), spacing 12 ]
--         [ newLogPanel model
--         , el [ Font.size 14 ] (text <| model.message)
--         , el [ Font.size 11 ] (text <| "Server: " ++ Configuration.backend)
--         ]
--
--
--
-- STYLE
--


viewLogs : Model -> Element Msg
viewLogs model =
    column [ spacing 12, padding 20, height (px 400) ]
        [ el [ Font.size 16, Font.bold ] (text "Logs")
        , indexedTable
            [ spacing 4, Font.size 12 ]
            { data = Log.filter model.logFilterString model.logs
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 40
                  , view = \k log -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                  }
                , { header = el [ Font.bold ] (text "Name")
                  , width = px 80
                  , view = \k log -> el [ Font.size 12 ] (logNameButton model.maybeCurrentLog log)
                  }
                ]
            }
        ]


logNameButton : Maybe Log -> Log -> Element Msg
logNameButton currentLog log =
    Input.button (Style.titleButton (currentLog == Just log))
        { onPress = Just (GetEvents log.id)
        , label = Element.text log.name
        }


indexWidth : AppMode -> Int
indexWidth appMode =
    case appMode of
        Logging ->
            30

        Editing ->
            60


indexButton : Model -> Int -> Event -> Element Msg
indexButton model k event =
    case model.appMode of
        Logging ->
            el [ Font.size 12 ] (text <| String.fromInt <| k + 1)

        Editing ->
            setCurrentEventButton model event k


setCurrentEventButton : Model -> Event -> Int -> Element Msg
setCurrentEventButton model event index =
    Input.button (Style.titleButton (Just event == model.maybeCurrentEvent))
        { onPress = Just (SetCurrentEvent event)
        , label = el [ Font.bold ] (Element.text <| String.fromInt index ++ ": " ++ String.fromInt event.id)
        }
