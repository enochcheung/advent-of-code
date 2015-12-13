import Data.List.Split
import Data.Maybe
import Data.Set (Set, insert, size, singleton, union)
import Linear.V2

unriffle :: [a] -> ([a],[a])
unriffle = unriffle' ([], [])
    where
        unriffle' (a,b) [] = (a,b)
        unriffle' (a,b) (x:xs) = (\ (c,d) -> (x:c,d)) $ unriffle'' (a, b) xs
        unriffle'' (a,b) [] = (a,b)
        unriffle'' (a,b) (x:xs) = (\ (c,d) -> (c,x:d)) $unriffle' (a, b) xs


origin :: V2 Int
origin = V2 0 0

solve :: String -> Int 
solve s = size $ union santa_visited robosanta_visited
        where
            (santa_visited, _) = foldl walk ((singleton origin), origin) santa_inst
            (robosanta_visited, _) = foldl walk ((singleton origin), origin) robosanta_inst
            (santa_inst, robosanta_inst) = unriffle $ map c2diff s
            walk :: (Set (V2 Int), V2 Int) -> V2 Int -> (Set (V2 Int), V2 Int)
            walk (visited, pos) diff = ((insert (pos + diff) visited ), (pos + diff))

c2diff :: Char -> V2 Int
c2diff '<' = V2 (-1) 0
c2diff '>' = V2 1 0
c2diff '^' = V2 0 1
c2diff 'v' = V2 0 (-1)
c2diff  _  = V2 0 0

main :: IO ()
main = do
    l <- getLine
    putStrLn $ show $ solve l

