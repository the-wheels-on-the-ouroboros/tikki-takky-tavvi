module UtilitiesTests (all) where

import ElmTest exposing (Test, assert, assertEqual, suite, test)

import Utilities exposing (maximumBy)


all : Test
all =
    suite "Using general-purpose functions"

        [ suite "Getting the maximum element from a list when translated into comparable values"

            [ test "Returns nothing if given an empty list" <|
                assertEqual Nothing (maximumBy identity [])

            , test "Returns the element that has the max value when given function is applied" <|
                assertEqual (Just 0) (maximumBy ((*) -1) [0, 1, 2, 3])
            ]
        ]
