module Main where

import GHC.Conc ( numCapabilities, pseq )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Conc.Sync (par, pseq, numCapabilities)

riemannBlock :: Double -> Double -> (Double -> Double) -> Double
riemannBlock intA intB f
  | intA > intB     = riemannBlock intB intA f
  | otherwise       = (f intA + f intB) / 2.0 * (intB-intA)

riemannIteratorPar :: [Double] -> (Double -> Double) -> Bool -> Int -> [Double]
riemannIteratorPar [a] _ _ _ = []
riemannIteratorPar (p:q:r) f paral num
    | paral && num <= numCapabilities = (tailR `par` headR) `pseq` (headR : tailR)
    | otherwise = headR:tailR
    where 
        headR = riemannBlock p q f
        tailR = riemannIteratorPar (q:r) f paral (num + 1)

riemannSum :: [Double] -> (Double -> Double) -> Bool -> Double
riemannSum partition f paral = sum (riemannIteratorPar partition f paral 1)

createPartition :: (Double, Double) -> Double -> [Double]
createPartition (intA, intB) n
  | intA > intB = createPartition (intB, intA) n
  | otherwise   = [intA, intA+(intB-intA)/n .. intB]

integralIterator :: Double -> Double -> (Double, Double) -> (Double -> Double) -> Double -> Bool -> Double
integralIterator lastInt lastN (intA, intB) f prec paral
  | prec > abs (lastInt - currentInt) = currentInt
  | otherwise                         = integralIterator currentInt currentN (intA, intB) f prec paral
  where currentN = lastN * 2
        currentInt = riemannSum (createPartition (intA, intB) currentN) f paral

integral :: (Double, Double) -> (Double -> Double) -> Bool -> Double
integral (intA, intB) f paral = let prec = 0.00001
                           in integralIterator (riemannSum [intA, intB] f paral) 1 (intA, intB) f prec paral

main :: IO ()
main = do    
    putStrLn "Sequential"
    start <- getCurrentTime
    print $ integral (0, 500*3.14159) (\x -> x*sin x) False
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
    putStrLn "number of cores: 1"

    putStrLn "Parallel"
    start <- getCurrentTime
    print $ integral (0, 500*3.14159) (\x -> x*sin x) True
    
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
    putStrLn $ "number of cores: " ++ show numCapabilities