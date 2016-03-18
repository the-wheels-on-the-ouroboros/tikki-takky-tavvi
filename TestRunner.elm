module Main where

import Signal exposing (Signal)

import ElmTest exposing (Test, consoleRunner, suite)
import Console exposing (IO, run)
import Task

import GameLogic.ComputerPlayerTests
import GameLogic.GameEndTests
import GameLogic.UpdateStateTests
import GameModelTests
import UtilitiesTests

tests : Test
tests =
  suite "Tic Tac Toe tests"
    [ GameLogic.ComputerPlayerTests.all
    , GameLogic.GameEndTests.all
    , GameLogic.UpdateStateTests.all
    , GameModelTests.all
    , UtilitiesTests.all
    ]

console : IO ()
console = consoleRunner tests

port runner : Signal (Task.Task x ())
port runner = run console
