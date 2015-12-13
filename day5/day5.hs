has_3_vowels :: String -> Bool
has_3_vowels = has_vowels 3 
    where
        has_vowels 0 _ = True
        has_vowels n (x:xs) = case (is_vowel x) of
                                True -> has_vowels (n-1) xs
                                False -> has_vowels n xs
        has_vowels n _ = False
        is_vowel c = elem c "aeiou"

has_double :: String -> Bool
has_double = (any (uncurry (==))) . pairs

no_bad_pair :: String -> Bool
no_bad_pair = (all (not.is_bad_pair)) . pairs

is_bad_pair :: (Char, Char) -> Bool
is_bad_pair (a,b) = elem w ["ab", "cd", "pq", "xy"] 
    where w = [a,b]

s_tail :: [a] -> [a]
s_tail [] = []
s_tail xs = tail xs

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (s_tail xs)

nice_string :: String -> Bool
nice_string s = (has_3_vowels s) && (has_double s) && (no_bad_pair s)

main :: IO ()
main = do
    ls <- getContents
    putStrLn $ show $ length $ filter nice_string $ lines ls