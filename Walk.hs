module Main where

import Data.Maybe         ( fromJust )

-- | Fsharp-style forward pipe operator
(|>) :: a -> (a -> b) -> b
(|>) a fn = fn a

-- | Return the list of all permutations of a list.
permutations [a] = [[a]]
permutations xs = concatMap f xs
  where
      f x = [x:ys | ys <- filter (/= x) xs |> permutations]

type Graph = [(Char, (Int, Int))]

coalesce :: Eq t => (t, t) -> (t, t) -> Either () t
coalesce (a, b) (c, d)
    | a == c || b == c = Right d
    | a == d || b == d = Right c
    | otherwise     = Left ()

walk :: Graph -> String -> Bool
walk graph = go 0
  where
      go _ [] = True
      go node (e:es) | 0 == node  = go a es || go b es
                     | node == a  = go b es
                     | node == b  = go a es
                     | otherwise = False
        where (a, b) = fromJust $ lookup e graph

-------------------------------------------------------------------------------

graph :: Graph
graph =
    [ ('a', (1, 2))
    , ('b', (2, 3))
    , ('c', (1, 3))
    , ('d', (3, 4))
    , ('e', (3, 4))
    , ('f', (1, 4))
    ]

tests :: Bool
tests = w "abcfde"
      && w "cdfabe"
      && w "cedbaf"
      && w "fdbace"
      && w "defcba"
      && w "dcabef"
      && w "cbafde"
      && not (w "dfecba")
      && not (w "abdcfe")
      && not (w "badcfe")
      && not (w "cabdef")
      && not (w "cbadef")
      && not (w "decafb")
      && not (w "edcbaf")
  where
    w = walk graph

main :: IO ()
main = print $ length $ filter (walk graph) $ permutations "abcdef"

