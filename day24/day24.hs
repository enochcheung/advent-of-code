import Data.List

main :: IO ()
main = do
        contents <- getContents
        let
            packages = map read $ lines contents
            ans1 = solve packages
            ans2 = solve2 packages
        print ans1
        print ans2

-- returns a list s such that s!!i = list of all subsets of size i
-- subsets "abc" == [[""],["a","b","c"],["ab","ac","bc"],["abc"],[],[],[],...]
subsets :: [a] -> [[[a]]]
subsets []      = [[]]:(repeat [])
subsets (x:xs)  = zipWith (++) ([]:(map (map (x:)) subxs)) subxs
                where
                    subxs = subsets xs

-- Checks if a subset of l that is a third of the sum can be extended to three subsets of 
-- equal sums. Note that this was not needed for the solution of the problem, since it turns
-- out that the best first subset without this restriction still satisifies this requirement
validGrpThirds :: [Int] -> [Int] -> Bool
validGrpThirds l = (not.null) . (filter ((==(div (sum l) 3)).sum)) . subsequences . (l \\)

validGrpQuarters :: [Int] -> [Int] -> Bool
validGrpQuarters l = (not.null) . (filter ((==(div (sum l) 4)).sum)) . subsequences . (l \\)

thirdSubsets :: [Int] -> [[[Int]]]
thirdSubsets l = map (filter (validGrpThirds l)) $ map (filter ((==(div (sum l) 3)).sum)) $ subsets l

quarterSubsets :: [Int] -> [[[Int]]]
quarterSubsets l = map (filter (validGrpQuarters l)) $ map (filter ((==(div (sum l) 4)).sum)) $ subsets l

solve :: [Int] -> Maybe Int
solve = (fmap minimum) . (fmap (map product)) . (find (not.null)) . thirdSubsets

solve2 :: [Int] -> Maybe Int
solve2 = (fmap minimum) . (fmap (map product)) . (find (not.null)) . quarterSubsets


