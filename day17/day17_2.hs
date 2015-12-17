import Data.List (find)

main :: IO ()
main = do
            contents <- getContents
            let
                containers = map read $ lines contents
                ans = solve 150 containers
            print $ fmap length ans


-- returns a list s such that s!!i = list of all subsets of size i
subsets :: [a] -> [[[a]]]
subsets []      = [[]]:(repeat [])
subsets (x:xs)  = zipWith (++) ([]:(map (map (x:)) subxs)) subxs
                where
                    subxs = subsets xs

solve :: Int -> [Int] -> Maybe [[Int]]
solve n = (find (not.null)).(map (filter ((==n).sum))).subsets 