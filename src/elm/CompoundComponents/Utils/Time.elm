module CompoundComponents.Utils.Time exposing
    ( days
    , differenceInSeconds
    , hours
    , minutes
    , posixToSeconds
    , seconds
    , subtractMilliFromPosix
    , subtractMilliFromPosixAsSeconds
    )

import Time


seconds : Int
seconds =
    1


minutes : Int
minutes =
    60 * seconds


hours : Int
hours =
    60 * minutes


days : Int
days =
    24 * hours


posixToSeconds : Time.Posix -> Int
posixToSeconds time =
    let
        ms =
            Time.posixToMillis time

        msFloat =
            toFloat ms / 1000
    in
    floor msFloat


differenceInSeconds : Time.Posix -> Time.Posix -> Int
differenceInSeconds end start =
    posixToSeconds end - posixToSeconds start


subtractMilliFromPosix : Time.Posix -> Int -> Time.Posix
subtractMilliFromPosix startTime subtractAmountMilli =
    let
        timeMs =
            Time.posixToMillis startTime

        newTimeMs =
            timeMs - subtractAmountMilli
    in
    Time.millisToPosix newTimeMs


subtractMilliFromPosixAsSeconds : Time.Posix -> Int -> Int
subtractMilliFromPosixAsSeconds startTime subtractAmountMilli =
    let
        timeMs =
            Time.posixToMillis startTime

        newTimeMs =
            timeMs - subtractAmountMilli
    in
    newTimeMs
        |> toFloat
        |> (\millis -> millis / 1000)
        |> round
