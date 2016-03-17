module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import ModelTests
import GameLogic.GameEndTests
import GameLogic.NegamaxTests
import GameLogic.UpdateStateTests
import UtilitiesTests

tests : Test
tests =
  suite "Tic Tac Toe tests"
    [ GameLogic.GameEndTests.all
    , GameLogic.NegamaxTests.all
    , GameLogic.UpdateStateTests.all
    , ModelTests.all
    , UtilitiesTests.all
    ]

console : IO ()
console = consoleRunner tests

port runner : Signal (Task.Task x ())
port runner = run console
