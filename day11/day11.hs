import qualified Data.Set as Set

nextPw :: String -> String
nextPw pw = head $ filter validPw $ map numToPw [((pwToNum pw)+1)..]


digitToChar :: Int -> Char 
digitToChar = ("abcdefhjkmnpqrstuvwxyz" !!)

charToDigit :: Char -> Int
charToDigit c = length $ takeWhile (/= c) "abcdefhjkmnpqrstuvwxyz"

trueCharToDigit :: Char -> Int
trueCharToDigit c = length $ takeWhile (/= c) "abcdefghijklmnopqrstuvwxyz"

numToPw :: Int -> String
numToPw n = (take padAmount (repeat 'a')) ++ pwTail
        where
            padAmount = max 0 (8 - (length pwTail))
            pwTail = numToPw' [] n
            numToPw' s 0 = s
            numToPw' s n = numToPw' ((digitToChar r):s) q
                where
                    (q, r) = divMod n 22

pwToNum :: String -> Int
pwToNum pw = pwToNum' (reverse pw)
            where
                pwToNum' "" = 0
                pwToNum' (x:xs) = 22*(pwToNum' xs) + (charToDigit x)

-- Does not check for no i,o,l, since those chars will not be enumerated
validPw :: String -> Bool
validPw pw = (twoPairs pw) && (hasIncTrip pw)

twoPairs :: String -> Bool
twoPairs = distinct2 . (filter (uncurry (==))) . pairs

distinct2 :: Eq a => [a] -> Bool
distinct2 = (any (uncurry (/=))).pairs


-- hasMatchDoubles :: String -> Bool
-- hasMatchDoubles pw = case charPairs of
--                         (pr:prs)  -> helper Set.empty pr prs
--                         otherwise -> False
--             where
--                 charPairs = pairs pw
--                 helper :: Set.Set Char -> (Char, Char) -> [(Char, Char)] -> Bool
--                 helper _ _ []     = False
--                 helper s (p,q) ((x,y):xs) = if (x == y && Set.member x s) then True else (helper newS (x,y) xs)
--                                     where
--                                         newS = if (p == q) then (Set.insert p s) else s


hasIncTrip :: String -> Bool
hasIncTrip pw = any isIncTrip $ triples pw
            where
                isIncTrip (a,b,c) = (bd == (ad + 1)) && (cd == (bd + 1))
                    where
                        ad = trueCharToDigit a 
                        bd = trueCharToDigit b
                        cd = trueCharToDigit c

pairs :: [a] -> [(a,a)]
pairs s = zip s (drop 1 s)

triples :: [a] -> [(a,a,a)]
triples s = zip3 s (drop 1 s) (drop 2 s)



