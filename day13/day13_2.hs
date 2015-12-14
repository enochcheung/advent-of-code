import Text.Regex.PCRE
import Data.HashMap.Strict as HM
import Data.Set as S
import Prelude as P
import Data.List (permutations)
import Data.Hashable
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)

type Person = String

main :: IO ()
main = do
    c <- getContents
    let
        parsedC = P.map parseLine $ lines c
        m = edgeMap parsedC
        plist = people parsedC
        ans = maxHap2 m plist
    putStrLn $ show ans

parseLine :: String -> ((Person, Person), Int)
parseLine l = case match of
                [_, a, change, amount, b] -> ((a,b), if (change == "gain") then n else (-n))
                            where
                                n = read amount
                otherwise                 -> error "parse error"

        where
            pattern = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)\\."
            match = getAllTextSubmatches $ l =~ pattern :: [String]

edgeMap :: [((Person, Person), Int)] -> HashMap (Person,Person) Int
edgeMap = HM.fromList

people :: [((Person, Person), Int)] -> [Person]
people = S.toList . S.fromList . (P.map (fst.fst))

happiness :: HashMap (Person,Person) Int -> [Person] -> Int
happiness m plist = sum $ P.map getHap cycleEdges
        where
            cycleEdges = (last plist, head plist):(zip plist (drop 1 plist))
            getHap edge = (fromMaybe 0 $ HM.lookup edge m) + (fromMaybe 0 $ HM.lookup (swap edge) m)

cyclicPermutations :: [Person] -> [[Person]]
cyclicPermutations plist = P.map ((head plist):) $ permutations $ tail plist

maxHap :: HashMap (Person, Person) Int -> [Person] -> Int
maxHap m plist = maximum $ P.map (happiness m) $ cyclicPermutations plist

happiness2 :: HashMap (Person,Person) Int -> [Person] -> Int
happiness2 m plist = sum $ P.map getHap edges
        where
            edges = zip plist (drop 1 plist)
            getHap edge = (fromMaybe 0 $ HM.lookup edge m) + (fromMaybe 0 $ HM.lookup (swap edge) m)

maxHap2 :: HashMap (Person, Person) Int -> [Person] -> Int
maxHap2 m plist = maximum $ P.map (happiness2 m) $ permutations plist