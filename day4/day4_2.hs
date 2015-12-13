import qualified Data.ByteString as B
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)
import Data.Bits



md5_list :: String -> [B.ByteString]
md5_list secret = map (hash . pack) seeds
    where
        seeds = map (\ i -> secret ++ (show i)) [1..]

is_coin :: B.ByteString -> Bool
is_coin bs = case unpacked_w8 of
                [a,b,c] -> (a == 0) && (b == 0) && (c == 0)
                otherwise -> True
    where
        unpacked_w8 = B.unpack $ B.take 3 bs

least_coin :: String -> Int
least_coin secret = (+1) $ length $ takeWhile (not.is_coin) $ md5_list secret

