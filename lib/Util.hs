module Util where

import Data.Char

titleToSnakeCase :: String -> String
titleToSnakeCase = concatMap lowerscore

snakeToTitleCase :: String -> String
snakeToTitleCase =
  unwords . map capitalize . words . concatMap unlowerscore

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

unlowerscore :: Char -> String
unlowerscore '_' = " "
unlowerscore c = [c]

lowerscore :: Char -> String
lowerscore ' ' = "_"
lowerscore c = [toLower c]
