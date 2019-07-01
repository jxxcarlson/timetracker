module TestData exposing (e1, e2, log)

import Log exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)


e1 : Event
e1 =
    { id = 1
    , name = "Scales"
    , note = "--"
    , startTime = Time.millisToPosix 0
    , duration = TypedTime Minutes 11.4
    , insertedAt = Time.millisToPosix 0
    }


e2 : Event
e2 =
    { id = 1
    , name = "Finger exercises"
    , note = "--"
    , startTime = Time.millisToPosix 0
    , duration = TypedTime Minutes 4.1
    , insertedAt = Time.millisToPosix 0
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
