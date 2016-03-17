module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import MinimaxTests
import ModelTests
import TestHelpersTests
import UpdateTests

tests : Test
tests =
  suite "Tic Tac Toe tests"
    [ MinimaxTests.all
    , ModelTests.all
    , TestHelpersTests.all
    , UpdateTests.all
    ]

console : IO ()
console = consoleRunner tests

port runner : Signal (Task.Task x ())
port runner = run console
