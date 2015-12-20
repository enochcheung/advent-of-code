import Math.NumberTheory.Primes.Factorisation
import Data.List


main :: IO ()
main = do
    print $ brute1 34000000


presents :: Int -> Int
presents = (*10) . sum . factors

presents2 :: Int -> Int
presents2 n = (*11) $ sum $ filter (\x -> 50*x >= n) $ factors n

factors :: Int -> [Int]
factors = (foldr (\ (p,e) l -> [a*b | a<-(map (pow (fromIntegral p)) [0..e]), b<-l]) [1]) . factorise . fromIntegral


pow :: Int -> Int -> Int
pow a b = truncate $ (fromIntegral a) ** (fromIntegral b)

brute1 :: Int -> Int
brute1 n = 1 + (length $ takeWhile (<n) $ map presents [1..])

brute2 :: Int -> Int
brute2 n = 1 + (length $ takeWhile (<n) $ map presents2 [1..])