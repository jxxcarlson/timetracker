module Log exposing (Event, Log)

import Time exposing (Posix)
import TypedTime exposing (TypedTime)


type alias Log =
    { id : Int
    , name : String
    , note : String
    , userId : Int
    , data : List Event
    }


type Duration
    = Duration Float


type NaiveDateTime
    = NaiveDateTime String


type alias Event =
    { id : Int
    , name : String
    , note : String
    , startTime : Posix
    , duration : TypedTime
    , insertedAt : Posix
    }
