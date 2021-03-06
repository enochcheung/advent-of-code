import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Char (isLower)
import Prelude as P
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- fmap T.pack getContents
    let
        (d, input) = parse contents
        ans = length $ step d input
    print ans

parseLine :: Text -> (Text, Text)
parseLine = l2p . T.words
        where
            l2p [a,_,b] = (a,b)
            l2p x       = error ("parse error on "++(show x))

parse :: Text -> (Map Text [Text], Text)
parse contents = (d, input)
            where
                ls = T.lines contents
                d = toMultimap $ map parseLine $ take (length ls - 2) $ ls
                input = last ls

step :: Map Text [Text] -> Text -> Set Text
step d = (S.map T.concat) . f . splitOnCap . T.unpack
        where
            f []     = S.empty
            f (x:xs) = S.union (S.fromList $ map (:xs) subs) (S.map (x:) (f xs))
                    where
                        subs = Map.findWithDefault [] x d

splitOnCap :: String -> [Text]
splitOnCap ""            = []
splitOnCap [x]           = [T.pack [x]]
splitOnCap (x:xs@(y:ys)) = if (isLower y) then ((T.pack [x,y]):(splitOnCap ys)) else ((T.pack [x]):(splitOnCap xs))



toMultimap :: Ord a => [(a,b)] -> Map a [b]
toMultimap = (foldr (uncurry $ Map.insertWith (++)) Map.empty) . (map (over _2 (:[])))