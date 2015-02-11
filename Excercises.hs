module Main where

-- | Left fold (Already exists in Prelude, only included here for completeness.)
foldl_ _ acc [] = acc
foldl_ op acc (x:xs) = acc `op` foldl_ op x xs

-- | A list reverse is implemented in terms of a left fold.
reverse_ :: [a] -> [a]
reverse_ = foldl f [] where f a b = b:a

-- | Split a String into a list of words (words being defined as substrings
-- separated by a blank space in the original string). 
words_ :: String -> [String]
words_ = reverse_ . f [] []
  where
    f ys zs "" = reverse_ ys:zs
    f ys zs (x:xs) | ' ' == x  = f [] (reverse_ ys:zs) xs
                   | otherwise = f (x:ys) zs xs

-- | Given a String, generate a new string with the words of the original
-- string in reverse order, e.g., the string "What time is Bork" becomes
-- "Bork is time What".
backwards :: String -> String
backwards = unwords . reverse_ . words_

testBackwards str = do
   print $ "You said: " ++ str 
   print "---"
   print $ "I say: " ++ backwards str
   print "---"

--------------------------------------------------------------------------------

-- | A list; 2, 3, and all integers greater than 3 for which isPrime returns 
-- True (i.e., for which the predicate holds).
primes = 2 : [p | p <- [3..], isPrime p]

-- | "Trial division" algorithm -- for integers greater than 3, this function 
-- returns True if and only if the following statement holds for the argument q:
--
--   P(q) := There exists no prime number p; 2 <= p <= sqrt(q); such that p | q. 
-- 
-- As an example, here we check if 221 is prime:
--
--         2 | 221 -> False 
--         3 | 221 -> False
--
--        No need to check composite numbers. Why? 
--
--            Assume:  (NOT (a|b)) /\ (ac|b)    (e.g., [NOT 2|221] but 4|221)
--
--                     This means that there exists a number d s.t. acd = b.
--                     But then a|b, since cd is an integer and a(cd) = b,
--                     which contradicts the assumption (NOT a|b). 
--                     
--                     Now, [NOT ((NOT X) /\ Y))]  is equivalent to  Y ==> X
--                     so we have [ ac|b ==> a|b ].  Therefore we know that 
--                     any product of a number p for which we have already 
--                     asserted that it does not divide q, cannot divide q.
--
--         5 | 221 -> False
--         7 | 221 -> False
--
--               ...
--
--        13 | 221 -> True   ... At this point we terminate and return False.
--
--        
-- Let's try 17 instead:
--
--        2 | 17 -> False
--        3 | 17 -> False
--        4 | 17 -> False   ... and since 4 >= floor(sqrt(17)), we're done!
--
--
-- To justify why we only check divisibility for primes up to sqrt(q), first
-- let t = +sqrt(q). Then the list of numbers we consider for divisibility 
-- w.r.t. q can be written as:
--
--     [2, 3, ..., int(t), int(t)+1, ..., q]     ( minus composites )
--
-- Now, suppose there is a p > t that divides q. Then ps = q for some s, where 
-- s must be less than t, since (p > t /\ s >= t) implies ps > t*t = q. Then,
-- obviously s | q, but since we have already concluded that there is no s,
-- 2 <= s <= t which divides q, there cannot be a p > t such that p | q. QED. 

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime q = and [rem q n /= 0 | n <- takeWhile upToSqr primes]
  where
    upToSqr x = x^2 <= q

-- | List all primes [2 .. max]. Not as efficient as a straight-up imperative 
-- sieve using a mutable data structure, but more readable.
primesUpto max = takeWhile (<= max) primes

testPrimes = print (primesUpto 10000) >> print "---"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    testPrimes
    testBackwards "What time is Bork"
