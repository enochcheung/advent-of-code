main :: IO ()
main = do
        content <- getContents
        let
            ls = lines content
            diffs = map countDiff ls
            ans = sum diffs
        putStrLn $ show ans

countDiff :: String -> Int
countDiff s = (countDiff' 0 s) + 2
                where
                    countDiff' :: Int -> String -> Int
                    countDiff' n []             = n
                    countDiff' n ('\\':'\\':s)  = countDiff' (n+1) s
                    countDiff' n ('\\':'\"':s)  = countDiff' (n+1) s
                    countDiff' n ('\\':'x':s)   = countDiff' (n+3) (drop 2 s)
                    countDiff' n (c:s)          = countDiff' n s