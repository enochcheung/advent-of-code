floors' :: Int -> [Char] -> Maybe Int 
floors' n [] = Just n
floors' n (x:xs) = case x of
                  '('       -> floors' (n+1) xs
                  ')'       -> floors' (n-1) xs
                  otherwise -> Nothing


floors :: [Char] -> Maybe Int
floors = floors' 0


m2s :: (Show a) => Maybe a -> String
m2s (Just x) = show x
m2s Nothing = ""


main :: IO ()
main = do
       line <- getLine
       putStrLn $ m2s $ floors line