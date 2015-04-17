-- Download the dictionary

module Download where

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C

download :: IO ()
download = do
  request <- parseUrl dictUrl
  withManager $ \manager -> do
    response <- http request manager
    responseBody response C.$$+- sinkFile dictFile
      where
        dictUrl  = "http://www.gutenberg.org/cache/epub/29765/pg29765.txt"
        dictFile = "dict.txt"
