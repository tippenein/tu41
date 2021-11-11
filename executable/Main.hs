{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Typeable
import Transunion
import System.Environment
import Options.Generic

data Example
  = Example
  { source :: String
  } deriving (Generic)

instance ParseRecord Example

data Source
  = FilePath_ FilePath
  | Raw String
  deriving (Generic, Typeable, Read)

instance ParseRecord Source
instance ParseFields Source
instance ParseField Source


main :: IO ()
main = do
  Example { source }<- getRecord "TU41 parser"
  d <- readFile source
  print $ parseToJson d
