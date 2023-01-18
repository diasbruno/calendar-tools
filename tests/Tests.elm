module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time
import Time.Extra as Time

import Calendar exposing (..)

suite : Test
suite =
    describe "Calendar"
        [ describe "working with months"
              [ describe "edge of month"
                    [ test "first day of month" <|
                          \_ ->
                          let e = Time.Parts 2023 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                              d = Time.Parts 2023 Time.Jan 2 0 0 0 0 |> fromPartsUtc
                          in Expect.equal (beginOfMonth Time.utc d |> toPartsUtc) (toPartsUtc e)
                    , test "last day of month" <|
                        \_ ->
                        let e = Time.Parts 2023 Time.Jan 31 0 0 0 0 |> fromPartsUtc
                            d = Time.Parts 2023 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                        in Expect.equal (endOfMonth Time.utc d |> toPartsUtc) (toPartsUtc e)
                    ]
              , describe "single full month"
                  [ test "Jan 2023 single month" <|
                        \_ ->
                        let f = Time.Parts 2023 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                            e = List.range 1 31 ++ List.repeat 4 0
                            d = Time.Parts 2023 Time.Jan 10 0 0 0 0 |> fromPartsUtc
                        in Expect.equal (singleMonthCalendar Time.utc d) e
                  , test "Fev 2023 single month" <|
                      \_ ->
                      let f = Time.Parts 2023 Time.Feb 1 0 0 0 0 |> fromPartsUtc
                          e = List.repeat 3 0 ++ List.range 1 28 ++ List.repeat 4 0
                          d = Time.Parts 2023 Time.Feb 10 0 0 0 0 |> fromPartsUtc
                      in Expect.equal (singleMonthCalendar Time.utc d) e
                  ]
              , describe "full month with previous and next month dates"
                  [ test "Jan 2023 full month" <|
                        \_ ->
                        let f = Time.Parts 2023 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                            e = List.range 1 31 ++ [1, 2, 3, 4]
                            d = Time.Parts 2023 Time.Jan 10 0 0 0 0 |> fromPartsUtc
                        in Expect.equal (monthCalendar Time.utc d) e
                  , test "Fev 2023 full month" <|
                      \_ ->
                      let f = Time.Parts 2023 Time.Feb 1 0 0 0 0 |> fromPartsUtc
                          e = List.range 29 31 ++ List.range 1 28 ++ List.range 1 4
                          d = Time.Parts 2023 Time.Feb 10 0 0 0 0 |> fromPartsUtc
                      in Expect.equal (monthCalendar Time.utc d) e
                  ]
              ]
        , describe "working with weeks"
            [ describe "first week of month"
                  [ test "Jan 2023 has all" <|
                        \_ ->
                        let e = 7
                            d = Time.Parts 2023 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                        in Expect.equal (firstWeekOfMonth Time.utc d |> List.length) e
                  , test "Jan 2022 has only one" <|
                      \_ ->
                          let e = 1
                              d = Time.Parts 2022 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                          in Expect.equal (firstWeekOfMonth Time.utc d |> List.length) e
                  ]
            , describe "last week of month"
                [ test "Jan 2023 has all" <|
                      \_ ->
                      let e = 3
                          d = Time.Parts 2023 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                      in Expect.equal (lastWeekOfMonth Time.utc d |> List.length) e
                , test "Jan 2022 has only two" <|
                      \_ ->
                      let e = 2
                          d = Time.Parts 2022 Time.Jan 1 0 0 0 0 |> fromPartsUtc
                      in Expect.equal (lastWeekOfMonth Time.utc d |> List.length) e
                ]
            ]
        ]
