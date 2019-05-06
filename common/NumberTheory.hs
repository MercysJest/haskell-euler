module NumberTheory where

import Data.Maybe
import Data.List.Ordered

-- Pollard Rho Algorithm
-- Comment: has some type of memory leak, try out
-- http://neilmitchell.blogspot.com/2015/09/detecting-space-leaks.html
-- maybe do bang patterns?

prg :: Integral a => a -> a
prg x = x^2 + 1

prIter :: Integral a => a -> (a,a,a) -> (a,a,a)
prIter n (x,y,_) = (x', y', d')
  where x' = prg x
        y' = (prg . prg) y
        d' = gcd (abs (x'-y')) n

pollardRho :: Integral a => a -> (a,a) -> Maybe a
pollardRho n (x,y) = if getD result /= n then Just (getD result) else Nothing
  where getD (_, _, lst) = lst
        end tup = getD tup /= 1 || getD tup == n
        result = until end (prIter n) (x,y,1)

prAttempt :: Integral a => Int -> a -> Maybe a
prAttempt itr n = if null tries then Nothing else head tries
  where tries = take itr $ filter isJust $ map (pollardRho n) (zip [2..] (repeat 2))

-- Sieve of Eratosthenes
primes :: Integral a => [a]
primes = 2:3:minus [5, 7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

-- Trial Division Factorization

trialFactor' :: Integral a => a -> a -> [a]
trialFactor' !n !p
 | p' == n = [p']
 | otherwise = p':(trialFactor' (n `div` p') p')
   where p' = head $ [x | x <- [p, p+st..], n `mod` x == 0]
         st = if p == 2 then 1 else 2

trialFactor :: Integral a => a -> [a]
trialFactor n = trialFactor' n 2
