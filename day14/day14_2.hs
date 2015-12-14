import Text.Regex.PCRE
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import qualified Data.Set as S
import Data.Set (Set)

data Reindeer = Reindeer {name :: String, speed :: Int, duration :: Int, rest :: Int} deriving (Eq, Ord, Show)

main :: IO ()
main = do
        contents <- getContents
        let
            reindeers = map parseLine $ lines contents
            ans = winner reindeers 2503
        print ans

distanceTraveled :: Int -> Reindeer -> Int
distanceTraveled t reindeer = q*(speed reindeer)*(duration reindeer) + (min r (duration reindeer))*(speed reindeer)
                        where
                            (q,r) = divMod t ((duration reindeer)+(rest reindeer))

parseLine :: String -> Reindeer
parseLine l = case match of
                [_, nm, s, d, r] -> Reindeer {name=nm, speed=(read s), duration=(read d), rest=(read r)}
                otherwise        -> error "parse error"
            where
                pattern = "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds\\."
                match = getAllTextSubmatches $ l =~ pattern :: [String]

furthestSet :: [Reindeer] -> Int -> Set Reindeer
furthestSet reindeers t = maxBySet (distanceTraveled t) reindeers

maxBySet :: (Ord a, Ord b) => (a -> b) -> [a] -> Set a 
maxBySet f as = foldr cmp S.empty as
            where
                cmp a bs = if (S.null bs) then (S.singleton a) else
                                let b = S.findMax bs in 
                                case (compare (f a) (f b)) of
                                    LT -> bs
                                    GT -> S.singleton a
                                    EQ -> S.insert a bs

getCounts :: Ord a => [Set a] -> Map a Int
getCounts = foldr incSet (Map.empty)
            where
                inc :: Ord a => a -> Map a Int -> Map a Int
                inc a mp = Map.insertWith (+) a 1 mp
                incSet :: Ord a => Set a -> Map a Int -> Map a Int
                incSet as mp = S.foldr inc mp as

mostFrequentCount :: Ord a => [Set a] -> Int
mostFrequentCount = maximum . Map.elems . getCounts

winner :: [Reindeer] -> Int -> Int
winner reindeers t = mostFrequentCount $ map (furthestSet reindeers) [1..(t+1)]


