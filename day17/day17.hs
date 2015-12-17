main :: IO ()
main = do
            contents <- getContents
            let
                containers = map read $ lines contents
                ans = (solve containers)!!150
            print ans


solve :: [Int] -> [Int]
solve []     = 1:(repeat 0)
solve (c:cs) = zipWith (+) ss ((replicate c 0)++ss)
            where
                ss = solve cs