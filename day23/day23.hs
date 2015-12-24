import Control.Lens
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

data Reg = A | B deriving (Show)

type Registers = (Integer, Integer)

data Instruction = Hlf Reg | Tpl Reg | Inc Reg | Jmp Int | Jie Reg Int | Jio Reg Int
                deriving (Show)

main :: IO ()
main = do
    contents <- getContents
    let
        ins = mkIns contents
        ans = view (regLens B) $ exec ins
        ans2 = view (regLens B) $ exec2 ins
    print ans
    print ans2
    
regLens A = _1
regLens B = _2

apply :: Instruction -> (Registers, Int) -> (Registers, Int)
apply (Hlf r)   = (over _2 (+1)) . (over (_1 . (regLens r)) (flip div 2))
apply (Tpl r)   = (over _2 (+1)) . (over (_1 . (regLens r)) (*3))
apply (Inc r)   = (over _2 (+1)) . (over (_1 . (regLens r)) (+1))
apply (Jmp n)   = (over _2 (+n))
apply (Jie r n) = \(rs, i) -> if (even (view (regLens r) rs)) then (rs, i+n) else (rs, i+1)
apply (Jio r n) = \(rs, i) -> if ((view (regLens r) rs)==1) then (rs, i+n) else (rs, i+1)

exec :: Vector Instruction -> Registers
exec ins = exec' ((0,0), 0)
    where
        exec' (rs, i)
                | i<0 || (length ins) <= i      = rs
                | otherwise                     = exec' $ apply (ins ! i) (rs,i)

exec2 :: Vector Instruction -> Registers
exec2 ins = exec' ((1,0), 0)
    where
        exec' (rs, i)
                | i<0 || (length ins) <= i      = rs
                | otherwise                     = exec' $ apply (ins ! i) (rs,i)


mkIns :: String -> Vector Instruction
mkIns = V.fromList . (map parseIns) . lines


parseIns :: String -> Instruction
parseIns l = case (words l) of
                ["hlf", r] -> Hlf (readReg r)
                ["tpl", r] -> Tpl (readReg r)
                ["inc", r] -> Inc (readReg r)
                ["jmp", i] -> Jmp (readSigned i)
                ["jie", r, i] -> Jie (readReg r) (readSigned i)
                ["jio", r, i] -> Jio (readReg r) (readSigned i)
                otherwise     -> error ("parse error on instruction "++l)

readReg :: String -> Reg
readReg "a"  = A
readReg "b"  = B
readReg "a," = A
readReg "b," = B

readSigned :: String -> Int
readSigned ('+':i) = read i
readSigned ('-':i) = - (read i)

