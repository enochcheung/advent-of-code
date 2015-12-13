import qualified Data.Aeson as A
import Data.ByteString.Lazy (pack)
import Data.ByteString.Internal (c2w)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Vector as V

main :: IO ()
main = do
    c <- getContents
    let
        jsonValues = mapMaybe fromJString $ lines c
        ans = sum $ map getSum jsonValues
    putStrLn $ show ans


getSum :: A.Value -> Int 
getSum (A.Object m)   = if (hasRed m) then 0 else (sum $ map getSum $ Map.elems m)
getSum (A.Array v)    = V.sum $ V.map getSum v
getSum (A.String _)  = 0
getSum (A.Number sci) = fromMaybe 0 $ toBoundedInteger sci
getSum (A.Bool _)     = 0
getSum (A.Null)    = 0


hasRed :: A.Object -> Bool
hasRed = (not.null) . (Map.filter (== (A.String (T.pack "red"))))

fromJString :: String -> Maybe A.Value
fromJString = A.decode . pack . (map c2w)

