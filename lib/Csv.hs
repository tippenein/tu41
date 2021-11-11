{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Csv where

import qualified Data.ByteString.Lazy as LB
import Data.Csv as Csv
import Data.List (nub)
import qualified Data.Vector as V
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

data TUData
  = TUData {
    _tuFFRCode    :: !String
  , _fieldName    :: !String
  , _displacement :: !Int
  , _length       :: !Int
  , _type         :: !String
  } deriving (Generic, Show, Eq)

instance Ord TUData where
  (TUData _ _ displacement1 _ _) `compare` (TUData _ _ displacement2 _ _) =
    displacement1 `compare` displacement2

instance FromRecord TUData where

readCsv :: IO [TUData]
readCsv = do
  csvData <- LB.readFile "TU41_full_csv_data_file.csv"
  let datas = Csv.decode HasHeader csvData :: Either String (V.Vector TUData)
    in case datas of
      Left _ -> fail "bad csv format"
      Right m -> return $ V.toList m

{-# NOINLINE allSignals #-}
allSignals :: [String]
allSignals = unsafePerformIO (nub . map _tuFFRCode <$> readCsv)
