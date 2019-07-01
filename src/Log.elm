module Log exposing (DateFilter(..), Event, EventGrouping(..), Log, dateFilter, eventSum, filter, groupingFilter)

import DateTime exposing (NaiveDateTime(..))
import List.Extra as LE
import Time exposing (Posix)
import TypedTime exposing (TypedTime(..), Unit(..))


type alias Log =
    { id : Int
    , name : String
    , note : String
    , userId : Int
    , data : List Event
    }


type EventGrouping
    = NoGrouping
    | GroupByDay


type DateFilter
    = NoDateFilter
    | FilterByLast Int


type Duration
    = Duration Float


type alias Event =
    { id : Int
    , name : String
    , note : String
    , startTime : NaiveDateTime
    , duration : TypedTime
    , insertedAt : NaiveDateTime
    }


filter : String -> List Log -> List Log
filter filterString logs =
    List.filter (\log -> String.contains (String.toLower filterString) (String.toLower log.name)) logs


dateFilter : String -> DateFilter -> List Event -> List Event
dateFilter todayAsString dateFilter_ eventList =
    case dateFilter_ of
        NoDateFilter ->
            eventList

        FilterByLast k ->
            List.filter (\event -> DateTime.inLastNDaysBeforeDate todayAsString k (DateTime.naiveDateTimeValue event.insertedAt) == Just True) eventList


eventSum : List Event -> TypedTime
eventSum eventList =
    eventList
        |> List.map .duration
        |> TypedTime.sum


groupingFilter : Int -> EventGrouping -> List Event -> List Event
groupingFilter timeZoneOffset eventGrouping eventList =
    case eventGrouping of
        NoGrouping ->
            correctTimeZone timeZoneOffset eventList

        GroupByDay ->
            eventsByDay timeZoneOffset eventList


eventsByDay : Int -> List Event -> List Event
eventsByDay timeZoneOffset list =
    list
        |> List.map (\r -> offsetTimeZone timeZoneOffset r)
        |> timeSeries
        |> timeSeriesRD
        |> List.sortBy Tuple.first
        |> fillGaps ( NaiveDateTime "", 0 )
        |> group
        |> List.map sumList2


correctTimeZone : Int -> List Event -> List Event
correctTimeZone timeZoneOffset list =
    list
        |> List.map (\r -> offsetTimeZone timeZoneOffset r)


offsetTimeZone : Int -> Event -> Event
offsetTimeZone offset event =
    let
        (NaiveDateTime str) =
            event.insertedAt
    in
    { event | insertedAt = NaiveDateTime (DateTime.offsetDateTimeStringByHours offset str) }


timeSeries : List Event -> List ( NaiveDateTime, Float )
timeSeries eventList =
    eventList
        |> List.map (\event -> ( event.insertedAt, event.duration |> TypedTime.convertToSeconds ))


timeSeriesRD : List ( NaiveDateTime, Float ) -> List ( Int, ( NaiveDateTime, Float ) )
timeSeriesRD listOfPairs =
    List.map augmentPair listOfPairs


augmentPair : ( NaiveDateTime, Float ) -> ( Int, ( NaiveDateTime, Float ) )
augmentPair ( ndt, f ) =
    ( rataDie ndt, ( ndt, f ) )


filterValues : List ( a, Maybe b ) -> List ( a, b )
filterValues =
    List.foldr foldrValues []


foldrValues : ( a, Maybe b ) -> List ( a, b ) -> List ( a, b )
foldrValues pair list =
    case pair of
        ( _, Nothing ) ->
            list

        ( item, Just v ) ->
            ( item, v ) :: list


group : List ( a, b ) -> List (List ( a, b ))
group list =
    list
        |> LE.groupWhile (\x y -> Tuple.first x == Tuple.first y)
        |> List.map (\( u, v ) -> u :: v)


fillGaps : a -> List ( Int, a ) -> List ( Int, a )
fillGaps default list =
    let
        fillGap start end =
            List.range (start + 1) (end - 1)
                |> List.map (\i -> ( i, default ))

        ii : Int
        ii =
            Maybe.map Tuple.first (List.head list) |> Maybe.withDefault 0
    in
    List.foldl
        (\(( i, a ) as item) ( last, acc ) ->
            if i == last then
                ( last, item :: acc )

            else if i == last + 1 then
                ( last + 1, item :: acc )

            else
                ( i, item :: fillGap last i ++ acc )
        )
        ( ii, [] )
        list
        |> Tuple.second
        |> List.reverse


sumList2 :
    List ( Int, ( NaiveDateTime, Float ) )
    -> Event
sumList2 list =
    let
        head =
            List.head list

        index : Int
        index =
            Maybe.map Tuple.first head |> Maybe.withDefault -1

        tuple =
            Maybe.map Tuple.second head

        dt : NaiveDateTime
        dt =
            Maybe.map Tuple.first tuple |> Maybe.withDefault (NaiveDateTime "2000-01-010T00:00:00")

        sum : Float
        sum =
            List.map (Tuple.second >> Tuple.second) list |> List.sum
    in
    { id = index + 1
    , name = "foo"
    , note = "--"
    , startTime = dt
    , duration = TypedTime Seconds sum
    , insertedAt = dt
    }


sumList : List ( Int, Float ) -> ( Maybe Int, Float )
sumList list =
    let
        sum =
            list |> List.map Tuple.second |> List.sum

        index =
            List.head list |> Maybe.map Tuple.first
    in
    ( index, sum )


rataDie : NaiveDateTime -> Int
rataDie (NaiveDateTime str) =
    DateTime.rataDieFromNaiveDateTime str - 737148
