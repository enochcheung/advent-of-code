import Data.Maybe (mapMaybe, fromMaybe)
import Data.Word (Word16)
import Data.Bits
import qualified Data.Map.Lazy as Map
import Text.Read (readMaybe)

data Instruction = Instruction Source String deriving (Show)

data Source = Assign Wire
            | Shift Int Wire
            | And Wire Wire
            | Or Wire Wire
            | Not Wire
            deriving (Show)

data Wire = Name String | Value Word16 deriving (Show)

main :: IO ()
main = do
        content <- getContents
        let
            instList = parseList content
            m = makeMap instList
            part1_ans = Map.lookup "a" m
            m2 = makePart2Map (fromMaybe (error "part 1 wire a not found") part1_ans) instList
            ans = Map.lookup "a" m2
        putStrLn $ fromMaybe "wire not found" (fmap show ans)

parse :: String -> Instruction
parse s = case ws of
            [a, "->", dest]              -> Instruction (Assign (readWire a)) dest
            [a, "RSHIFT", i, "->", dest] -> Instruction (Shift (- (read i)) (readWire a)) dest
            [a, "LSHIFT", i, "->", dest] -> Instruction (Shift (read i) (readWire a)) dest
            [a, "AND", b, "->", dest]    -> Instruction (And (readWire a) (readWire b)) dest
            [a, "OR", b, "->", dest]     -> Instruction (Or (readWire a) (readWire b)) dest
            ["NOT", a, "->", dest]       -> Instruction (Not (readWire a)) dest
            otherwise                 -> error ("unparsable "++s)
        where
            ws = words s
            readWire wire = case (readMaybe wire :: Maybe Word16) of
                            Nothing -> Name wire
                            Just value -> Value value

parseList :: String -> [Instruction]
parseList = (map parse) . lines

makeMap :: [Instruction] -> Map.Map String Word16
makeMap instructions = m
        where
        m = Map.fromList $ map (\ (Instruction source dest) -> (dest, (evalSource m source))) instructions

makePart2Map :: Word16 -> [Instruction] -> Map.Map String Word16 
makePart2Map part1a instructions = m
        where
        m = Map.fromList $ map instToPair instructions
        instToPair (Instruction source "b")  = ("b", part1a)
        instToPair (Instruction source dest) = (dest, (evalSource m source))

-- creates a thunk for evaluating the value using the map, which due to laziness should
-- result in memoization
evalSource :: Map.Map String Word16 -> Source -> Word16
evalSource m source = case source of
            Assign b        -> evalWire b
            Shift i b       -> shift (evalWire b) i
            And b c         -> (evalWire b) .&. (evalWire c)
            Or b c          -> (evalWire b) .|. (evalWire c)
            Not b           -> complement (evalWire b)
        where
            evalWire (Value i) = i
            evalWire (Name n) = fromMaybe (error "wire not found") (Map.lookup n m)



