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
import Data.Tuple (swap)
import Data.Function (on)


main :: IO ()
main = do
    contents <- fmap T.pack getContents
    let
        (d, target) = parseR contents
        reductions = iterate (greedyStep d) target
        distance = length $ takeWhile (/="e") reductions
    print distance

parseLine :: Text -> (Text, Text)
parseLine = l2p . T.words
        where
            l2p [a,_,b] = (a,b)
            l2p x       = error ("parse error on "++(show x))

bfs :: Ord a => (a-> Set a) -> a -> a -> Int
bfs getChildren = (bfs' 0 S.empty) . (S.singleton)
    where
        bfs' n seen currLevel target = if (S.member target currLevel) then n else
                                                    bfs' (n+1) (S.union seen currLevel) nextLevel target 
            where
                nextLevel = S.unions $ map getChildren $ S.toList currLevel


parseR :: Text -> (Map Text Text, Text)
parseR contents = (d, target)
            where
                ls = T.lines contents
                d = Map.fromList $ map swap $ map parseLine $ take (length ls - 2) $ ls
                target = last ls

parseRF :: IO (Map Text Text, Text)
parseRF = do
            f <- fmap T.pack (readFile "test3.txt")
            return $ parseR f

splitOnCap :: String -> [Text]
splitOnCap ""            = []
splitOnCap [x]           = [T.pack [x]]
splitOnCap (x:xs@(y:ys)) = if (isLower y) then ((T.pack [x,y]):(splitOnCap ys)) else ((T.pack [x]):(splitOnCap xs))

greedyStep :: Map Text Text -> Text -> Text
greedyStep d = (minimumBy (on compare T.length)) . S.toList . (step d)

step :: Map Text Text -> Text -> Set Text
step d = f
        where
            f curr = S.fromList $ Map.foldrWithKey (\ k v l -> (replace1All k v curr)++l) [] d

replace1 :: Text -> Text -> Text -> Text
replace1 needle replacement haystack = T.concat [a, replacement, c]
                    where
                        (a,bc) = T.breakOn needle haystack
                        c = T.drop (T.length needle) bc


replace1All :: Text -> Text -> Text -> [Text]
replace1All needle replacement haystack = map makeString breaks
                    where
                        breaks = T.breakOnAll needle haystack
                        makeString (a,bc) = T.concat [a, replacement, T.drop (T.length needle) bc]




