module Tests where

import ElmTest exposing (..)

import UpdateTests

all : Test
all =
  suite "Tic Tac Toe tests"
    [
      UpdateTests.all
    ]
