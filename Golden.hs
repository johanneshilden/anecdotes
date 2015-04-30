module Main where

goldenRatio seed n = last f
  where
    f = seed:[1 + 1/f !! x | x <- [0 .. n]]

main :: IO ()
main = print $ goldenRatio 1 100
