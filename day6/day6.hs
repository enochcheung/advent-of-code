import Text.Regex.Posix
import Data.Maybe (mapMaybe)


data Range = Range Int Int Int Int deriving (Show)
data Pos = Pos Int Int deriving (Show)
data Operation = TurnOn | TurnOff | Toggle | None deriving (Show)



main :: IO()
main = do
    c <- getContents
    putStrLn $ show $ countLights $ parseToFunctions c


countLights :: [(Pos -> Int -> Int)] -> Int
countLights updateMaps = sum $ map (computeFinalPosition updateMaps) gridOfIndex

applyOperation :: Operation -> Range -> Pos -> Int -> Int
applyOperation op range pos a = if (rangeCheck range pos) then (update a) else a
                            where
                                update a = case op of
                                    TurnOn  -> 1
                                    TurnOff -> 0
                                    Toggle  -> (1-a)
                                    None -> a


rangeCheck :: Range -> Pos -> Bool
rangeCheck (Range x1 y1 x2 y2) (Pos x y) = (x1 <= x && x <= x2 && y1 <= y && y <= y2) 
 

gridOfIndex :: [Pos]
gridOfIndex = [ (Pos x y) | x <- [0..999], y <- [0..999]]

computeFinalPosition :: [(Pos -> Int -> Int)] -> Pos -> Int
computeFinalPosition ls pos = foldl (\ x f -> f pos x) 0 ls

parse :: String -> Maybe (Operation, Range)
parse s = case grps of
            [op', x1', y1', x2', y2'] -> Just (readOp op', Range (read x1') (read y1') (read x2') (read y2'))
            otherwise                 -> Nothing
    where
        r = "(turn off|turn on|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
        (_, _, _, grps) = s =~ r :: (String, String, String, [String])
        readOp op' = case op' of
            "turn on" -> TurnOn
            "turn off" -> TurnOff
            "toggle" -> Toggle
            otherwise -> None


parseToFunctions :: String -> [(Pos -> Int -> Int)]
parseToFunctions = (map (uncurry applyOperation)). (mapMaybe parse) . lines




