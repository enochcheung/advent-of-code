floors' :: Int -> Int -> [Char] -> Maybe Int 
floors' n m [] = if (n < 0) then Just m else Nothing
floors' n m (x:xs) 
               | n < 0 = Just m
               | otherwise = case x of
                  '('       -> floors' (n+1) (m+1) xs
                  ')'       -> floors' (n-1) (m+1) xs
                  otherwise -> Nothing


floors :: [Char] -> Maybe Int
floors = floors' 0 0


m2s :: (Show a) => Maybe a -> String
m2s (Just x) = show x
m2s Nothing = ""


main :: IO ()
main = do
       line <- getLine
       putStrLn $ m2s $ floors line