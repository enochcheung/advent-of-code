import Text.Regex.PCRE


data Reindeer = Reindeer {name :: String, speed :: Int, duration :: Int, rest :: Int} deriving (Show)

main :: IO ()
main = do
        contents <- getContents
        let
            reindeers = map parseLine $ lines contents
            ans = furthest reindeers 2503
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

furthest :: [Reindeer] -> Int -> Int
furthest reindeers t = maximum $ map (distanceTraveled t) reindeers