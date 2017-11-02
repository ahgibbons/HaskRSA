module Main where

import Lib
import PrimeGen
import Criterion.Main

main :: IO ()
main = mainTest


mainTest :: IO ()
mainTest = defaultMain [
    bgroup "PrimeGen1" [ 
                
                bench "300-50" $ nfIO (genPrimeIO 50 (10^300) (10^310))
              , bench "300" $ nfIO (genPrimeIO 60 (10^300) (10^310))],
    
    bgroup "PrimeGen3" [
                
                bench "300-50" $ nfIO (genPrimeIO3 50 (10^300) (10^310))
              , bench "300" $ nfIO (genPrimeIO3 60 (10^300) (10^310))]]

