module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Log exposing (..)
import Style
import TestData exposing (..)
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


type EventGrouping
    = NoGrouping
    | GroupByDay


type DateFilter
    = NoDateFilter
    | FilterByLast Int


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
    , currentLog : Maybe Log
    , currentEvent : Maybe Event
    , logFilterString : String
    }


type Msg
    = NoOp
    | GetEvents Int


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , logs = [ TestData.log ]
      , currentLog = Just TestData.log
      , currentEvent = Just TestData.e1
      , logFilterString = ""
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetEvents id ->
            ( model, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainView model)


mainView : Model -> Element Msg
mainView model =
    column (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ -- filterPanel model
          logListPanel model
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
-- eventPanel : Model -> Element Msg
-- eventPanel model =
--     case model.currentEventList of
--         Nothing ->
--             Element.none
--
--         Just eventList_ ->
--             let
--                 today =
--                     Utility.DateTime.naiveDateStringFromPosix sharedState.currentTime
--
--                 events2 =
--                     dateFilter today model.dateFilter eventList_
--
--                 events =
--                     case model.filterState of
--                         NoGrouping ->
--                             Data.correctTimeZone model.timeZoneOffset events2
--
--                         GroupByDay ->
--                             Data.eventsByDay model.timeZoneOffset events2
--             in
--             column [ Font.size 12, spacing 36, moveRight 40, width (px 450) ]
--                 [ row [ moveLeft 40 ] [ Graph.barChart (gA model) (prepareData (getScaleFactor model) events) |> Element.html ]
--                 , row [ spacing 16 ]
--                     [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
--                     , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
--                     ]
--                 , newEventPanel 350 sharedState model
--                 ]
--
--
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
                  , view = \k log -> el [ Font.size 12 ] (logNameButton model.currentLog log)
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
