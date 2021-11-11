{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parser where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec

import Csv
import Util

type Parser = Parsec String ()

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

endOfLineOrInput :: Parser ()
endOfLineOrInput = char '\n' *> return () <|> eof

anySignal :: Parser (String, String)
anySignal = do
  signal <- signalParser <?> "a Signal"
  let total = sum $ map _length $ schemaForSignal signal

  content <- count total anyToken <?> show total <> " characters in segment for " <> signal
  return (signal, content)

signalParser :: Parser String
signalParser = choice (fmap try $ string <$> allSignals) <?> "Signal string"

parseSignal :: String -> [(String, String)]
parseSignal input =
  case parse' (many1 anySignal) input of
    Left e -> error $ show e
    Right r -> r

reconcileSegments :: String -> [(String, [(String, String)])]
reconcileSegments = map reconcileSegment . parseSignal

reconcileSegment :: (String, String) -> (String, [(String, String)])
reconcileSegment seg =
  let (signal,content) = seg
      fields = map (titleToSnakeCase . _fieldName) schema
      schema = schemaForSignal signal
      displacements = map _length schema
      contents = cutSegments displacements content
   in
     (signal, zip fields contents)

cutSegments :: [Int] -> String -> [String]
cutSegments [] _ = []
cutSegments _ "" = error "ran out of string to parse"
cutSegments (n:rest) content =
  take n content : cutSegments rest (drop n content)

schemaForSignal :: String -> [TUData]
schemaForSignal signal =
  drop 1 $ sort $ -- ignore type info, we already know it
    unsafePerformIO (filter ((==) signal . _tuFFRCode) <$> readCsv)
