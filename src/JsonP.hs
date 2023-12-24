module JsonP where

import Control.Applicative
import Data.Char
import qualified Data.Map as Map
import Parser

data JsonBaseType
  = JStr String
  | JNumber Int
  | JDouble Float
  | JBool Bool
  | JArray [JsonBaseType]
  deriving (Show)

-- Utils

whitespaces = many $ matchOne [" ", "\t", "\r", "\n"]

separator = whitespaces *> matchString "," <* whitespaces

-- Base Types Parsing

pBase = oneOf [pNum, pBool, pStr, pArray]

pNum = JNumber . read <$> parseWhile isDigit

pFloat = JDouble . read <$> parseWhile (\c -> isDigit c || '.' == c)

-- pFloat = (f <$> (pNum <* matchString ".")) <*> pNum
--   where
--     f (JNumber w) (JNumber d) = _a

pBool = toBool <$> matchOne ["true", "false"]
  where
    toBool "true" = JBool True
    toBool "false" = JBool False

pStr = JStr <$> (matchString "\"" *> parseWhile (/= '\"') <* matchString "\"")

pArray = JArray <$> (matchString "[" *> whitespaces *> many (pBase <* separator) <* matchString "]")

-- Copound Types
