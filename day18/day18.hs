import Data.Array.Repa ((:.))
import qualified Data.Array.Repa as R


type Board = R.Array R.U R.DIM2 Bool


main :: IO ()
main = do
    contents <- getContents
    let
        board = parseBoard contents
        finalBoard = (iterate step board)!!100
        ans = countLights finalBoard
    print ans


parseBoard :: String -> Board
parseBoard s = R.fromListUnboxed sh $ map (=='#') chars
        where
            chars = filter (/='\n') s
            n = truncate $ sqrt $ fromIntegral $ length chars :: Int
            sh = R.ix2 n n

step :: Board -> Board
step a = R.computeS $ R.traverse a id update
        where
            R.Z R.:. x R.:. y = R.extent a
            update prev (R.Z R.:. i R.:. j) = case (prev (R.ix2 i j)) of
                                            True  -> (2 <= n && n <= 3)
                                            False -> (n==3)
                where
                    inbound i j = 0 <= i && i < x && 0 <= j && j < y
                    neighbors = [prev (R.ix2 (i+a) (j+b)) | a<-[-1..1], b<-[-1..1], (a/=0 || b/=0), inbound (i+a) (j+b)]
                    n = length $ filter id neighbors


countLights :: Board -> Int
countLights = (R.foldAllS (+) 0) . (R.map (\ b -> if b then 1 else 0))