module PrimeGen where

import System.Random
import Crypto.Random
import Crypto.Random.DRBG
import Control.Monad.CryptoRandom
import Data.Binary
import Data.Bits
import qualified Data.ByteString as BS

data PublicKey  = PublicKey Integer Integer deriving (Show,Eq)
data PrivateKey = PrivateKey Integer Integer deriving (Show,Eq)

defaultExp = 65537

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

fermatLittleTheoremTest :: Integer -> Integer -> Bool
fermatLittleTheoremTest a p = (a^(p-1) `mod` p) == 1

fermatTest :: Integer -> Integer -> [(Integer,Bool)]
fermatTest p n = [(a,fermatLittleTheoremTest a p) | a <- [1..n], a `rem` p /= 0]

fermatPrimeTest :: StdGen -> Int -> Integer -> Bool
fermatPrimeTest g k n = all (\a -> modExp a (n-1) n == 1) as
  where
    as = take k $ randomRs (2,n-2) g

totient :: Integer -> Integer -> Integer
totient p q = lcm (p-1) (q-1)

-- Find GCD of two numbers plus the Coefficients of Bezout's Identity.
-- Used to find modular inverse.
euclideanAlg :: Integer -> Integer -> (Integer, Integer, Integer)
euclideanAlg a b 
  | b > a     = tripFlip $ euclideanAlg2 b 1 0 a 0 1
  | otherwise = euclideanAlg2 a 1 0 b 0 1
  where
    tripFlip (a,b,c) = (a,c,b)
    euclideanAlg2 rk0 sk0 tk0 0 sk1 tk1 = (rk0,sk0,tk0)
    euclideanAlg2 rk0 sk0 tk0 rk1 sk1 tk1 = 
        let qi = rk0 `div` rk1 in
        euclideanAlg2 rk1 sk1 tk1 (rk0 - qi*rk1) (sk0 - qi*sk1) (tk0 - qi*tk1)

-- Modular inverse, d, of a such that a.d = 1 mod m
modMultInv :: Integer -> Integer -> Integer
modMultInv m a = let (r,_,d) = euclideanAlg m a
                 in d `mod` m


bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

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
                                   

-----------------
-- Secure Random Number Generator
-----------------


genPrime :: CryptoRandomGen g => g -> Int -> Integer -> Integer -> Integer
genPrime g k minPrime maxPrime = head $ filter (fermatPrimeTest g' k) ns
  where
    Right (i,g'') = crandom g
    g'      = mkStdGen i
    ns      = crandomRs (minPrime, maxPrime) g''

genPrime2 :: CryptoRandomGen g => g -> Int -> Integer -> Integer -> Integer
genPrime2 g k minPrime maxPrime = head $ filter (fermatPrimeTest g' k) ns'
  where
    Right (i,g'') = crandom g
    g'      = mkStdGen i
    Right (n,_) = crandomR (minPrime, maxPrime) g''
    ns      = iterate ((+) 2) (n .|. 1) 
    ns'     = [n' | n' <- ns, n `rem` 5 /= 0 && n `rem` 3 /= 0]

genPrime3 :: CryptoRandomGen g => g -> Int -> Integer -> Integer -> Integer
genPrime3 g k minPrime maxPrime = head $ filter (fermatPrimeTest g' k) ns
  where
    Right (i,g'') = crandom g
    g'      = mkStdGen i
    Right (n,_) = crandomR (minPrime, maxPrime) g''
    ns      = iterate ((+) 2) (n .|. 1) 

genPrimeIO :: Int -> Integer -> Integer -> IO Integer
genPrimeIO k minPrime maxPrime = do
    g <- newGenIO :: IO HashDRBG
    return $ genPrime g k minPrime maxPrime

genPrimeIO2 :: Int -> Integer -> Integer -> IO Integer
genPrimeIO2 k minPrime maxPrime = do
    g <- newGenIO :: IO HashDRBG
    return $ genPrime2 g k minPrime maxPrime

genPrimeIO3 :: Int -> Integer -> Integer -> IO Integer
genPrimeIO3 k minPrime maxPrime = do
    g <- newGenIO :: IO HashDRBG
    return $ genPrime3 g k minPrime maxPrime

genPQ :: CryptoRandomGen g => g -> Int -> Integer -> Integer -> (Integer,Integer)
genPQ g k minPrime maxPrime = (p,q)
  where
    Right (g1,g2) = splitGen g
    p = genPrime g1 k minPrime maxPrime
    q = genPrime g2 k minPrime maxPrime
