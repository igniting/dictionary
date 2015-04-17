import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Char
import           Data.Maybe
import           Database.KeyValue     as KV
import           System.IO

data Record = Record String String

getRecord :: [String] -> (Maybe Record, [String])
getRecord = getValue . getTerm . ignoreComment

ignoreComment :: [String] -> [String]
ignoreComment = dropWhile (not . isTerm)

getTerm :: [String] -> (Maybe String, [String])
getTerm [] = (Nothing, [])
getTerm s = (Just (head s), tail s)

getValue :: (Maybe String, [String]) -> (Maybe Record, [String])
getValue (Nothing, rest) = (Nothing, rest)
getValue (Just t, rest) = (Just (Record t (unlines def)), rest') where
  (def, rest') = break isTerm rest

getRecords :: [String] -> [Record]
getRecords [] = []
getRecords s = let (maybeRecord, rest) = getRecord s in case maybeRecord of
                    Nothing -> getRecords rest
                    (Just r) -> r : getRecords rest

insertRecordsInDB :: String -> IO ()
insertRecordsInDB s = do
  m <- KV.initDB (Config "db")
  let records = getRecords $ lines s
  mapM_ (insertRecordInDB m) records
  KV.closeDB m

insertRecordInDB:: KeyValue -> Record -> IO ()
insertRecordInDB db (Record t def) = do
  v <- KV.get db (pack t)
  KV.put db (pack t) (B.append (fromMaybe B.empty v) (pack def))

isTerm :: String -> Bool
isTerm s = all isUpper (filter isAlpha s) && not (null s)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> printUsage >> getOption >>= performAction

printUsage :: IO ()
printUsage = putStrLn "1. Initialize the dictionary" >>
             putStrLn "2. See definition" >>
             putStr "Enter Option [1-2]: "

getOption :: IO Int
getOption = fmap read getLine

performAction :: Int -> IO ()
performAction 1 = do
  putStr "Enter the filename: "
  filename <- getLine
  contents <- readFile filename
  insertRecordsInDB contents

performAction 2 = do
  putStr "Enter a term: "
  term <- getLine
  m <- KV.initDB (Config "db")
  v <- KV.get m (pack $ map toUpper term)
  case v of
       Nothing -> putStrLn "Term not found in dictionary"
       Just b -> putStrLn (unpack b)

performAction _ = error "Invalid option."
