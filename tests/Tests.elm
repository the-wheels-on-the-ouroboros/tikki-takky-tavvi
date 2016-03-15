module Tests where

import ElmTest exposing (..)

import ModelTests
import UpdateTests

all : Test
all =
  suite "Tic Tac Toe tests"
    [
      ModelTests.all,
      UpdateTests.all
    ]
