-- Download the dictionary

module Download where

import           Control.Monad         (unless)
import           Crypto.Hash.MD5       (hashlazy)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.Conduit          as C
import           Data.Conduit.Binary   (sinkFile)
import           Network.HTTP.Conduit
import           System.Directory      (doesFileExist)

verify :: FilePath -> B.ByteString -> IO Bool
verify file hash = do
  b <- doesFileExist file
  if b then do
       putStrLn "dict.txt already exists, verifying hash..."
       h <- calculateHash file
       putStrLn $ if h == hash then "hash matched for dict.txt."
                               else "hash for dict.txt does not match."
       return $ h == hash
       else return False

calculateHash :: FilePath -> IO B.ByteString
calculateHash = fmap hashlazy . L.readFile

download :: IO ()
download = do
  b <- verify dictFile dictHash
  unless b $ do
    putStrLn "Downloading dict.txt..."
    request <- parseUrl dictUrl
    withManager $ \manager -> do
      response <- http request manager
      responseBody response C.$$+- sinkFile dictFile
  where
    dictUrl  = "http://www.gutenberg.org/cache/epub/29765/pg29765.txt"
    dictFile = "dict.txt"
    dictHash = B.pack [199,52,198,9,114,87,226,110,6,55,155,15,42,8,96,150]
