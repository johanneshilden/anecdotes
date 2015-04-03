module Main where

-- | Split a list into two based on whether the elements satisfy the given
--   predicate.
splip :: (a -> Bool) -> [a] -> ([a], [a])
splip pr xs = foldr fn ([], []) xs
  where
    fn x (ls, rs) | pr x      = (x:ls, rs)
                  | otherwise = (ls, x:rs)

-- | Naive quicksort (using first element as pivot).
qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = let (ls, rs) = splip (<x) xs in qs ls ++ x:(qs rs)

main :: IO ()
main = print $ qs [5,3,2,8,1,3,5,4,6,11,3,7,9,1,2,6,11,4,100,5]

