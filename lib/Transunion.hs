module Transunion where

import qualified Data.Map as Map
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS

import Util
import Parser
import Csv

parse :: String -> [(String, [ (String, String) ])]
parse = map reconcileSegment . parseSignal

parseToJson :: String -> BS.ByteString
parseToJson = encode . Map.map Map.fromList . Map.fromList . parse

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
