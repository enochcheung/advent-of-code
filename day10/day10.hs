lookAndSay40 :: String -> Int
lookAndSay40 seed = length $ (iterate lookAndSay seed) !! 40

lookAndSay50 :: String -> Int
lookAndSay50 seed = length $ (iterate lookAndSay seed) !! 50

lookAndSay :: String -> String
lookAndSay [] = []
lookAndSay (x:xs) = lookAndSay' x 1 xs
                where
                    lookAndSay' :: Char -> Int -> String -> String
                    lookAndSay' buf n [] = (show n)++[buf]
                    lookAndSay' buf n (x:xs) = if (x == buf) then (lookAndSay' buf (n+1) xs) else ((show n)++buf:(lookAndSay' x 1 xs)) 