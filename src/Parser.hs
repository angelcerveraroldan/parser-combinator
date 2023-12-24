module Parser
  ( matchChar,
    matchString,
    matchOne,
    parseWhile,
    parseIf,
    oneOf,
    Parser (..),
  )
where

import Control.Applicative
import Utils

data ParsingError
  = GenericError String
  | Unexpected
  | EOF
  | EmptyParser
  | EmptyString
  | BadPredicate
  deriving (Show)

newtype Parser a = Parser {run :: String -> Either ParsingError (a, String)}

instance Functor Parser where
  fmap f (Parser m) = Parser $ \str -> do
    (result, rest) <- m str
    Right (f result, rest)

instance Applicative Parser where
  pure a = Parser $ \str -> Right (a, str)

  (Parser m) <*> (Parser n) = Parser $ \str -> do
    (fn, rest') <- m str
    (val, rest) <- n rest'
    Right (fn val, rest)

instance Alternative Parser where
  empty = Parser $ const (Left EmptyParser)

  (Parser m) <|> (Parser n) = Parser $ \str -> case m str of
    Left err -> n str
    Right rs -> Right rs

-- Basic Parsers

parseIf :: (Char -> Bool) -> Parser Char
parseIf predicate = Parser mapIf
  where
    mapIf [] = Left EmptyString
    mapIf (c : cs)
      | predicate c = Right (c, cs)
      | otherwise = Left BadPredicate

matchChar char = parseIf (== char)

matchString :: String -> Parser String
matchString = traverse matchChar

-- Keep parsing characters until predicate is not met
parseWhile :: (Char -> Bool) -> Parser String
parseWhile predicate = Parser $ toResult <$> splitWhen predicate
  where
    toResult (met, unmet)
      | null met = Left BadPredicate
      | otherwise = Right (met, unmet)

-- Utils for parsers

-- | Generate a parser that will be sucessful if any of
--   the parseres in the given array are sucessful
oneOf :: [Parser a] -> Parser a
oneOf = foldl (<|>) empty

matchOne pattern = oneOf $ map matchString pattern
