{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fplugin=Plugin #-}

module Example where

{-# ANN type A "large-record" #-}
data A = A {a :: Int, b :: String}
  deriving stock (Show)

data B = B {a :: Int, b :: String}
  deriving stock (Show)

exampleA = A {b = "hello", a = 1}

transformB B {b = x} = B {b = x ++ "!", a = 10}
