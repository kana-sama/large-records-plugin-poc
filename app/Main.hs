{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin=Plugin #-}

{-# ANN type X "large-record" #-}
data X = X {a :: Int, b :: String}
  deriving stock (Show)

data Y = Y {a :: Int, b :: String}
  deriving stock (Show)

transformX X {b = x} = X {b = x ++ "!", a = 10}

transformY Y {b = x} = Y {b = x ++ "!", a = 10}

exampleX = X {b = "hello", a = 1}

exampleY = Y {b = "hello", a = 1}

main = do
  print (transformX exampleX)
  print (transformY exampleY)
