module TestData exposing (e1, e2, log)

import DateTime exposing (NaiveDateTime(..))
import Log exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)


e1 : Event
e1 =
    { id = 1
    , name = "Scales"
    , note = "--"
    , startTime = DateTime.NaiveDateTime <| DateTime.naiveDateStringFromPosix <| Time.millisToPosix 1561993648
    , duration = TypedTime Minutes 11.4
    , insertedAt = NaiveDateTime <| DateTime.naiveDateStringFromPosix <| Time.millisToPosix 1561993648
    }


e2 : Event
e2 =
    { id = 1
    , name = "Finger exercises"
    , note = "--"
    , startTime = NaiveDateTime <| DateTime.naiveDateStringFromPosix (Time.millisToPosix 1561993648)
    , duration = TypedTime Minutes 4.1
    , insertedAt = NaiveDateTime <| DateTime.naiveDateStringFromPosix <| Time.millisToPosix 1561993648
    }


type alias Log =
    { id : Int
    , name : String
    , note : String
    , userId : Int
    , data : List Event
    }


log =
    { id = 1
    , name = "Piano practice"
    , note = "Practice for recital"
    , userId = 1
    , data = [ e1, e2 ]
    }
