import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.List (permutations)

data City = City String deriving (Show, Eq, Ord)

main :: IO ()
main = do
    c <- getContents
    let
        m = makeEdgeMap c
        cities = cityList c
        ans = shortestTour m cities
    putStrLn $ show ans


makeEdgeMap :: String -> Map.Map (City, City) Int
makeEdgeMap c = Map.fromList $ map parse $ lines c

parse :: String -> ((City, City), Int)
parse s = case (words s) of
            [a, "to", b, "=", c] -> (((City a), (City b)), (read c))
            otherwise            -> error "parse error"

cityList :: String -> [City]
cityList s = Set.toList $ Set.fromList $ fstCity ++ sndCity
        where
            fstCity = map (fst.fst) edgeList
            sndCity = map (snd.fst) edgeList
            edgeList = map parse $ lines s

pathLength :: Map.Map (City, City) Int -> [City] -> Int
pathLength m cities = sum $ map getLength edges
                where
                    edges = zip cities (drop 1 cities)
                    getLength (c1, c2) = case (Map.lookup (c1, c2) m) of
                                        Just n -> n
                                        Nothing -> case (Map.lookup (c2, c1) m) of
                                                    Just n -> n
                                                    Nothing -> 999999


shortestTour :: Map.Map (City, City) Int -> [City] -> Int
shortestTour m cities = minimum $ map (pathLength m) $ permutations cities