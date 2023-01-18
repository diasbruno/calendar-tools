module Calendar exposing (..)

import Time
import Time.Extra as Time

type Calendar = Calendar

weekdayToInt w =
    case w of
        Time.Sun -> 0
        Time.Mon -> 1
        Time.Tue -> 2
        Time.Wed -> 3
        Time.Thu -> 4
        Time.Fri -> 5
        Time.Sat -> 6

toPartsUtc : Time.Posix -> Time.Parts
toPartsUtc = Time.posixToParts Time.utc

fromPartsUtc : Time.Parts -> Time.Posix
fromPartsUtc = Time.partsToPosix Time.utc

beginOfMonth : Time.Zone -> Time.Posix -> Time.Posix
beginOfMonth zone t =
    Time.posixToParts zone t |> \p -> Time.add Time.Day (1-p.day) zone t

nextMonth : Time.Zone -> Time.Posix -> Time.Posix
nextMonth zone t =
    Time.posixToParts zone t |> \p -> Time.add Time.Month 1 zone t

previousMonth : Time.Zone -> Time.Posix -> Time.Posix
previousMonth zone t =
    Time.posixToParts zone t |> \p -> Time.add Time.Month (-1) zone t

endOfMonth : Time.Zone -> Time.Posix -> Time.Posix
endOfMonth zone t =
    nextMonth zone t |> \t2 -> Time.posixToParts zone t2 |> \p -> Time.add Time.Day (-p.day) zone t2

firstWeekOfMonth : Time.Zone -> Time.Posix -> List Int
firstWeekOfMonth zone t =
    beginOfMonth zone t
        |> \t2 ->
            Time.posixToParts zone t2
                |> \p ->
                    let w = Time.toWeekday zone t2 |> weekdayToInt
                    in List.range 1 (7 - w)

lastWeekOfMonth : Time.Zone -> Time.Posix -> List Int
lastWeekOfMonth zone t =
    endOfMonth zone t
        |> \t2 ->
            Time.posixToParts zone t2
                |> \p ->
                    let w = Time.toWeekday zone t2 |> weekdayToInt
                        d = Time.toDay zone t2
                    in List.range (d - w) d

singleMonthCalendar : Time.Zone -> Time.Posix -> List Int
singleMonthCalendar zone d =
    let b = beginOfMonth zone d
        e = endOfMonth zone d
        bw = Time.toWeekday zone b |> weekdayToInt
        ew = Time.toWeekday zone e |> weekdayToInt
        l = if bw > 0 then List.repeat (6 - bw) 0 else []
        t = if ew > 0 then List.repeat (6 - ew) 0 else []
    in l ++ List.range 1 (Time.toDay zone e) ++ t

monthCalendar : Time.Zone -> Time.Posix -> List Int
monthCalendar zone d =
    let b = beginOfMonth zone d
        e = endOfMonth zone d
        bw = Time.toWeekday zone b |> weekdayToInt
        ew = Time.toWeekday zone e |> weekdayToInt
        l = if bw > 0 then (previousMonth zone d |> lastWeekOfMonth zone) else []
        t = if ew > 0 then (nextMonth zone d |> firstWeekOfMonth zone) else []
    in l ++ List.range 1 (Time.toDay zone e) ++ t
