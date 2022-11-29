{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)


main :: IO ()
main = runTests 
  [ probEval
  , probParse
  ]
