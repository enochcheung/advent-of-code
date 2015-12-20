{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Char (isLower)
import Prelude as P
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

main :: IO ()
main = do
    contents <- fmap T.pack getContents
    let
        (rules, target) = parse contents
        targetTokens = tokenize target

        -- Special tokens are Rn, Ar, Y which do not substituted
        numNonSpecial = countNonSpecial targetTokens
        numY = countY targetTokens

        -- Each substitution rule maps 1 token to 2+(num of Y) nonspecial tokens
        checkRules = all id $ map checkRule rules

        -- Therefore, we can consider each substution as increasing the number of non-special tokens by 1,
        -- with each Y in the result giving an additional non-special token
        ans = numNonSpecial - numY - 1
    putStrLn $ if checkRules then (show ans) else "rule assumption incorrect"

isSpecial :: Text -> Bool
isSpecial token = token=="Rn" || token=="Ar" || token=="Y"

countNonSpecial :: [Text] -> Int
countNonSpecial = length . (filter (not . isSpecial))

countY :: [Text] -> Int
countY = length . (filter (=="Y"))

checkRule :: (Text, Text) -> Bool
checkRule (k, v) = (length (tokenize k) == 1) && (2+ (countY (tokenize v)) == countNonSpecial (tokenize v))

parseLine :: Text -> (Text, Text)
parseLine = l2p . T.words
        where
            l2p [a,_,b] = (a,b)
            l2p x       = error ("parse error on "++(show x))

parse :: Text -> ([(Text, Text)], Text)
parse contents = (rules, target)
            where
                ls = T.lines contents
                rules = map parseLine $ take (length ls - 2) $ ls
                target = last ls

splitOnCap :: String -> [Text]
splitOnCap ""            = []
splitOnCap [x]           = [T.pack [x]]
splitOnCap (x:xs@(y:ys)) = if (isLower y) then ((T.pack [x,y]):(splitOnCap ys)) else ((T.pack [x]):(splitOnCap xs))

tokenize :: Text -> [Text]
tokenize = splitOnCap . T.unpack
