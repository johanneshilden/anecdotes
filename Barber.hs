module Main where

import Control.Monad.State.Strict

data Townsman = Customer | SelfShaver | Barber

-- The barber shaves precisely those men in town who do not shave themselves.
barberShaves :: Townsman -> State Int Bool
barberShaves man = do
    (stopAfterSteps 300 "not (not (not (not (not ... Too much recursion, guy!")
    liftM not (shavesHimself man)

-- Given a type of man, does this man shave himself?
shavesHimself :: Townsman -> State Int Bool
shavesHimself SelfShaver = return True
shavesHimself Customer   = return False
shavesHimself Barber     = barberShaves Barber

-- Recursion guard to prevent infinite loops.
stopAfterSteps :: Int -> String -> State Int ()
stopAfterSteps n msg = do
    a <- get
    if a > n
        then error msg
        else put (succ a)

-- barberShaves Barber
-- not (shavesHimself Barber)        -- replace Barber for x in r.h.s. of barberShaves x = not (shavesHimself x)
-- not (barberShaves Barber)         -- since shavesHimself Barber = barberShaves Barber
-- not (not (shavesHimself Barber))  -- once again, barberShaves x = not (shavesHimself x)
-- not (not (barberShaves Barber))
-- not (not (not (shavesHimself Barber)))
-- ...
-- not (not (not (not (not ...

askWhether :: Num s => t -> (t -> State s a) -> a
askWhether a b = fst $ runState (b a) 0

main :: IO ()
main = print
    ( askWhether Customer shavesHimself 
    , askWhether SelfShaver shavesHimself 
    , askWhether Barber shavesHimself )

