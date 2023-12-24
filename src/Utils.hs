module Utils where

x = 2

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen = splitWhen' []

splitWhen' :: [a] -> (a -> Bool) -> [a] -> ([a], [a])
splitWhen' acc predicate [] = (acc, [])
splitWhen' acc predicate (x : xs)
  | predicate x = splitWhen' (acc ++ [x]) predicate xs
  | otherwise = (acc, x : xs)
