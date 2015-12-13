import Data.List.Split
import Data.Maybe

parseLine :: String -> Maybe (Int, Int, Int)
parseLine l = case (splitOn "x" l) of
                [w, l, h]   -> Just (read w, read l, read h)
                otherwise   -> Nothing


computePaper :: (Int, Int, Int) -> Int
computePaper (w, l, h) = 2*a + 2*b + 2*c + m
    where
        a = l*w 
        b = w*h 
        c = h*l
        m = min a $ min b c


main :: IO()
main = do
    content <- getContents
    let
        l = lines content
        triples = mapMaybe parseLine l
        total = sum $ map computePaper triples
    putStrLn $ show total