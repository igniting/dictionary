-- | Parse the downloaded dictionary

module Parse where

import           Data.Char (isAlpha, isUpper)

data Record = Record String String

isTerm :: String -> Bool
isTerm s = all isUpper (filter isAlpha s) && not (null s)

ignoreComment :: [String] -> [String]
ignoreComment = dropWhile (not . isTerm)

getTerm :: [String] -> (Maybe String, [String])
getTerm [] = (Nothing, [])
getTerm s = (Just (head s), tail s)

getValue :: (Maybe String, [String]) -> (Maybe Record, [String])
getValue (Nothing, rest) = (Nothing, rest)
getValue (Just t, rest) = (Just (Record t (unlines def)), rest') where
  (def, rest') = break isTerm rest

getRecord :: [String] -> (Maybe Record, [String])
getRecord = getValue . getTerm . ignoreComment

getRecords :: [String] -> [Record]
getRecords [] = []
getRecords s = let (maybeRecord, rest) = getRecord s in case maybeRecord of
                    Nothing -> getRecords rest
                    (Just r) -> r : getRecords rest
