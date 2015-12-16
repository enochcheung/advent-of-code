import Text.Regex.PCRE

type Ingredient = (Int, Int, Int, Int, Int)

main :: IO ()
main = do
    contents <- getContents
    let
        ingredients = map parseLine $ lines contents
        recipes = partitions (length ingredients) 100
        bestScore = maximum $ map fst $ filter ((== 500).snd) $ map (scoreCal ingredients) recipes
    print bestScore

-- list of all partitions of m into sum of n numbers
partitions :: Int -> Int -> [[Int]]
partitions 1 m = [[m]]
partitions n m = [(x:xs) | x<-[0..m], xs<-(partitions (n-1) (m-x))]


scoreCal :: [Ingredient] -> [Int] -> (Int, Int)
scoreCal ingredients amounts = scoreCal' $ minZero5 $ foldr1 plus5 $ zipWith mult5 amounts ingredients
                    where
                        mult5 s (a,b,c,d,e) = (s*a,s*b,s*c,s*d,s*e)
                        plus5 (a,b,c,d,e) (a',b',c',d',e') = (a+a', b+b', c+c', d+d', e+e')
                        minZero5 (a,b,c,d,e) = (max 0 a, max 0 b, max 0 c, max 0 d, max 0 e)
                        scoreCal' (a,b,c,d,e) = (a*b*c*d, e)

parseLine :: String -> Ingredient
parseLine l = case match of
                    [_,name, a, b, c, d, e] -> (read a, read b, read c, read d, read e)
                    otherwise -> error "parseError"
              where
                pattern = "(\\w+): capacity (-?\\d), durability (-?\\d), flavor (-?\\d), texture (-?\\d), calories (-?\\d)"
                match = getAllTextSubmatches $ l =~ pattern
