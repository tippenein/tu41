module Parser where

import Text.Parsec
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort)
import Csv

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

schemaForSignal :: String -> [TUData]
schemaForSignal signal =
  drop 1 $ sort $ -- ignore type info, we already know it
    unsafePerformIO (filter ((==) signal . _tuFFRCode) <$> readCsv)
