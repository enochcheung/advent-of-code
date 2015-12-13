main :: IO ()
main = do
        content <- getContents
        let
            ls = lines content
            escapeCounts = map ((+ 2).countEscapeChars) ls
            ans = sum escapeCounts
        putStrLn $ show ans

countEscapeChars :: String -> Int
countEscapeChars = (length).(filter isEscapeChar)
        where
            isEscapeChar c = (c == '\"') || (c == '\\')