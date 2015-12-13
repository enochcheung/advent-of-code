import Text.Regex.PCRE

main :: IO ()
main = do
    c <- getContents
    let
        ans = sum $ map sum $ map getNums $ lines c
    putStrLn $ show ans


numRegex = "-?\\d+"

getNums :: String -> [Int]
getNums s = map read $ getAllTextMatches $ s =~ numRegex

