module NumberTheory where

import Data.Maybe
import Data.List
import Data.List.Ordered
import Data.Bits

-- Pollard Rho Algorithm
-- Comment: has some type of memory leak, try out
-- http://neilmitchell.blogspot.com/2015/09/detecting-space-leaks.html
-- maybe do bang patterns?

prg :: Integral a => a -> a
prg x = x*x + 1

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

-- Modular Exponentiation

mpow :: (Integral a, Bits a) => a -> a -> a -> a
mpow !c !e !m
  | e == 0 = 1
  | otherwise = (tmp * mpow (c'*c' `mod` m) (shiftR e 1) m) `mod` m
    where c' = c `mod` m
          tmp = if testBit e 0 then c' else 1

-- Miller Rabin Primality Test

mrWit :: (Integral a, Bits a) => a -> a -> a -> a -> Bool
mrWit s d n a = (mpow a d n == 1) || (n-1) `elem` vals
  where vals = [mpow a (2^r*d) n | r <- [0..s-1]]

mrExtract :: (Integral a, Bits a) => a -> (a,a)
mrExtract n = (genericLength br, shiftR (last br) 1)
  where br = takeWhile even $ iterate ((flip shiftR) 1) (n-1)

mRabin :: (Integral a, Bits a) => a -> Bool
mRabin n
  | n `elem` prs = True
  | n == 1 || or [n `mod` p == 0 | p <- prs] = False
  | otherwise = and [(mrWit s d n) a | a <-prs]
    where (s,d) = mrExtract n
          prs = [2,3,5,7,11,13,17,19,23,29]
