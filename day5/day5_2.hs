import qualified Data.Set as S
-- import qualified Data.Map.Strict as M

has_matching_pairs :: String -> Bool
has_matching_pairs l 
                     | (length l) < 4     = False
                     | otherwise          = (not . allDistinctWithBuffer . pairs) l

allDistinctWithBuffer :: Ord a => [a] -> Bool
allDistinctWithBuffer [] = True
allDistinctWithBuffer (x:xs) = adbw' S.empty x xs
            where
                adbw' :: Ord a => S.Set a -> a -> [a] -> Bool
                adbw' s buf [] = True
                adbw' s buf (x:xs) = if (S.member x s) then False
                                        else adbw' (S.insert buf s) x xs

has_good_triple :: String -> Bool
has_good_triple = (any is_good_triple).triples

is_good_triple :: Eq a => (a,a,a) -> Bool
is_good_triple (a,b,c) = a == c

triples :: [a] -> [(a,a,a)]
triples xs = zip3 xs (drop 1 xs) (drop 2 xs)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (drop 1 xs)

nice_string :: String -> Bool
nice_string s = (has_matching_pairs s) && (has_good_triple s)

main :: IO ()
main = do
    ls <- getContents
    putStrLn $ show $ length $ filter nice_string $ lines ls