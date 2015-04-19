-- | Database related operations

module DB where

import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (pack)
import           Data.Maybe
import           Database.KeyValue     as KV
import           Parse

insertRecordsInDB :: String -> IO ()
insertRecordsInDB s = do
  putStrLn "Inserting records in database..."
  m <- KV.initDB (Config "db")
  let records = getRecords $ splitLines s
  mapM_ (insertRecordInDB m) records
  KV.closeDB m
  putStrLn "Dictionary initialized."

insertRecordInDB:: KeyValue -> Record -> IO ()
insertRecordInDB db (Record t def) = do
  v <- KV.get db (pack t)
  KV.put db (pack t) (B.append (fromMaybe B.empty v) (pack def))

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
      isLineTerminator c = c == '\r' || c == '\n'
  in pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []
