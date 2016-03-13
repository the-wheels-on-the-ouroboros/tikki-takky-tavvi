module Greetings.HelloWorldTests where

import ElmTest exposing (..)

import Greetings.HelloWorld

all : Test
all =
  suite "Greetings.HelloWorld tests"
    [
      test "Greeting text is 'Hello, world!'"
        (assertEqual "Hello, world!" Greetings.HelloWorld.text)
    ]
