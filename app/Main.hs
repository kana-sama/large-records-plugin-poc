{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fplugin=Plugin #-}

import Example

{-# ANN type C "large-record" #-}
data C = C {a :: Int, b :: String}
  deriving stock (Show)

data D = D {a :: Int, b :: String}
  deriving stock (Show)

exampleC = C {b = "hello", a = 1}

exampleD = D {b = "hello", a = 1}

transformC C {b = x} = C {b = x ++ "!", a = 10}

transformD D {b = x} = D {b = x ++ "!", a = 10}

exampleB = B {b = "hello", a = 1}

-- TODO: fails because of resolving fields, which were already removed
-- transformA A {b = x} = A {b = x ++ "!", a = 10}

main = do
  -- print (transformA exampleA)
  print (transformB exampleB)
  print (transformC exampleC)
  print (transformD exampleD)
