module Main (main) where

import Parser
import System.Environment

main :: IO ()
main = do
  s <- getArgs
  print $ reconcileSegments $ head s

