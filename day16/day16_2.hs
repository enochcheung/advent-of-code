import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Text.Regex.PCRE
import Data.Maybe (fromMaybe)


type Sue = Map String Int

main :: IO ()
main = do
         contents <- getContents
         let
            sues = map parseLine $ lines contents
            misses = takeWhile (not.(matchSue reference)) sues
            winner = (length misses) + 1
         print winner


reference :: Sue
reference = Map.fromList [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

parseLine :: String -> Sue
parseLine l = Map.fromList $ map (\ [_,k,v] -> (k, read v)) match
               where
                 pattern = "(\\w+): (\\d+)"
                 match = l =~ pattern :: [[String]]

matchSue :: Sue -> Sue -> Bool
matchSue pattern sue = all (==True) $ Map.mapWithKey match' pattern
                     where
                       match' k v = fromMaybe True $ fmap (match'' k v) v'
                          where v' = Map.lookup k sue
                       match'' "cats" = (<)
                       match'' "trees" = (<)
                       match'' "pomeranians" = (>)
                       match'' "goldfish" = (>)
                       match'' _ = (==)
