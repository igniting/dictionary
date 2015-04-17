import           Data.ByteString.Char8 (pack, unpack)
import           Data.Char
import           Database.KeyValue     as KV
import           DB
import           Download
import           System.IO

main :: IO ()
main = hSetBuffering stdout NoBuffering >> printUsage >> getLine >>= performAction

printUsage :: IO ()
printUsage = putStrLn "1. Initialize the dictionary" >>
             putStrLn "2. See definition" >>
             putStr "Enter Option [1-2]: "

performAction :: String -> IO ()
performAction "1" = do
  putStrLn "Downloading the dictionary..."
  download
  putStrLn "Finished Downloading."
  contents <- readFile "dict.txt"
  putStrLn "Inserting terms in database..."
  insertRecordsInDB contents
  putStrLn "Dictionary initialized."

performAction "2" = do
  putStr "Enter a term: "
  term <- getLine
  m <- KV.initDB (Config "db")
  v <- KV.get m (pack $ map toUpper term)
  case v of
       Nothing -> putStrLn "Term not found in dictionary"
       Just b -> putStrLn (unpack b)

performAction _ = error "Invalid option."
