module UnsecPrimeGen where

import PrimeGen

-------------
-- Unsecure Random Number Generator used 
------------

unsecGenPrime :: StdGen -> Int -> Integer -> Integer -> Integer
unsecGenPrime g k minNum maxNum = head $ filter (fermatPrimeTest g k) ns
  where
    ns = randomRs (minNum, maxNum) g


unsecGenPrimeIO :: Int -> Integer -> Integer -> IO Integer
unsecGenPrimeIO k minPrime maxPrime = do
     g <- newStdGen
     return $ unsecGenPrime g k minPrime maxPrime

unsecGenPQ :: StdGen -> Int -> Integer -> Integer -> (Integer,Integer)
unsecGenPQ g k minNum maxNum = (p,q)
  where
    (g1,g2) = split g
    p = unsecGenPrime g1 k minNum maxNum
    q = unsecGenPrime g2 k minNum maxNum



-- genRSAKeys e g k minPrime maxPrime
-- e is public exponent, g is random seed,
-- k is number of iterations to run Rabin-Miller test.
-- minPrime, maxPrime is range to search for primes.
unsecGenRSAKeys :: Integer -> StdGen 
           -> Int -> Integer -> Integer 
           -> (PublicKey, PrivateKey)
unsecGenRSAKeys e g k minPrime maxPrime = let 
                                        (p,q) = unsecGenPQ g k minPrime maxPrime
                                        n    = p*q
                                        t    = lcm (p-1) (q-1)
                                        d    = modMultInv t e
                                     in (PublicKey n e, PrivateKey n d)
