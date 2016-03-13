module Tests where

import ElmTest exposing (..)

import String

import Greetings.HelloWorldTests

all : Test
all =
  suite "All Tests"
    [
      test "Example test" (assert True),
      Greetings.HelloWorldTests.all
    ]
