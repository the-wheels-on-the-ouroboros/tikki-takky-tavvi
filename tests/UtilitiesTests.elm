module UtilitiesTests (all) where

import ElmTest exposing (Test, assert, assertEqual, suite, test)

import Utilities exposing (areElementsEqual, count, maximumBy)


all : Test
all =
    suite "Using test helper functions"

        [ suite "Counting the occurence of elements in a list"

            [ test "Counts the occurence of given element in given list" <|
                assertEqual 3 (count 1 [0, 1, 2, 1, 1, 3, 3])

            , test "Returns zero if element is not in given list" <|
                assertEqual 0 (count 4 [0, 1, 2, 1, 1, 3, 3])
            ]
        , suite "Comparing equality of list elements ignoring order"

            [ test "Lists are unequal if they are of different lengths" <|
                assert (not (areElementsEqual [1, 2, 3, 3] [1, 2, 3]))

            , test "Lists are unequal if they have differing elements" <|
                assert (not (areElementsEqual [1, 2, 3, 3] [1, 2, 3, 4]))

            , test "Lists are unequal if they have differing numbers of the same elements" <|
                assert (not (areElementsEqual [1, 2, 3, 3] [1, 2, 2, 3]))

            , test "Lists are equal if they have the same elements and the same length" <|
                assert (areElementsEqual [0, 1, 2, 3] [3, 1, 0, 2])
            ]
        , suite "Getting the maximum element from a list when translated into comparable values"

            [ test "Returns nothing if given an empty list" <|
                assertEqual Nothing (maximumBy identity [])

            , test "Returns the element that has the max value when given function is applied" <|
                assertEqual (Just 0) (maximumBy ((*) -1) [0, 1, 2, 3])
            ]
        ]
