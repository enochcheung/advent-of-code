import Data.List.Split
import Data.Maybe
import Data.List (sort)

parseLine :: String -> Maybe (Int, Int, Int)
parseLine l = case (splitOn "x" l) of
                [w, l, h]   -> Just (read w, read l, read h)
                otherwise   -> Nothing


computeRibbon :: (Int, Int, Int) -> Int
computeRibbon (w, l, h) = 2*a + 2*b + w*l*h
    where
        [a,b] = take 2 $ sort [w,l,h]


main :: IO()
main = do
    content <- getContents
    let
        l = lines content
        triples = mapMaybe parseLine l
        total = sum $ map computeRibbon triples
    putStrLn $ show total