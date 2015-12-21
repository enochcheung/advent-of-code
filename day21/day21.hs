import Data.List
import Text.Regex.PCRE

type Hp = Int
type Damage = Int
type Armor = Int

data ItemStat = ItemStat {cost :: Int, damage :: Damage, armor :: Armor} deriving (Eq, Ord, Show)
data CharStat = CharStat {chp :: Hp, cdamage :: Damage, carmor :: Armor} deriving (Eq, Ord, Show)


main :: IO ()
main = do
        weapons <- readStatFile "weapons.txt"
        armors <- readStatFile "armors.txt"
        rings <- readStatFile "rings.txt"
        bossStat <- fmap readBoss $ readFile "input.txt"
        let
            louts = loadouts 100 weapons armors rings
            (winning, losing) = partition ((playerWins bossStat).snd) louts
            cheapestWin = minimum $ map fst winning
            mostExpensiveLoss = maximum $ map fst losing
        putStrLn $ (show cheapestWin)++"\n"++(show mostExpensiveLoss)

readBoss :: String -> CharStat
readBoss s = case (s =~ pattern) of
                [[_,h,d,a]] -> CharStat (read h) (read d) (read a)
                otherwise   -> error ("parse error on boss")
            where
                pattern = "Hit Points: (\\d+)\\nDamage: (\\d+)\\nArmor: (\\d+)"

readStatFile :: FilePath -> IO [ItemStat]
readStatFile = (fmap ((map parseStatLine).lines)) . readFile

parseStatLine :: String -> ItemStat
parseStatLine l = case (l =~ pattern :: [[String]]) of
                    [[_, c, d, a]] -> ItemStat (read c) (read d) (read a)
                    otherwise    -> error ("parse error on "++l)
                where
                    pattern = "\\w+ +(\\d+) +(\\d+) +(\\d+)"

playerWins :: CharStat -> CharStat -> Bool
playerWins (CharStat bossHp bossDamage bossArmor) (CharStat playerHp playerDamage playerArmor)
                    = (timeToKill playerDamage bossArmor bossHp) <= (timeToKill bossDamage playerArmor playerHp)
                    where
                        divUp a b = - (div (-a) b)
                        timeToKill damage armor hp = divUp hp (max 1 (damage-armor))

sumItems :: [ItemStat] -> ItemStat
sumItems = foldl' (\ (ItemStat a b c) (ItemStat a' b' c') -> ItemStat (a+a') (b+b') (c+c')) (ItemStat 0 0 0)

charStat :: Hp -> [ItemStat] -> (Int, CharStat)
charStat hp items = (cost, CharStat hp damage armor)
                where
                    ItemStat cost damage armor = sumItems items

loadouts :: Hp -> [ItemStat] -> [ItemStat] -> [ItemStat] -> [(Int, CharStat)]
loadouts hp weapons armors rings = [charStat hp (weapon:(armor ++ ring)) | weapon <- weapons, armor <- (choose1 armors), ring <- (choose2 rings)]

choose1 :: Eq a => [a] -> [[a]]
choose1 = ([]:) . (map (:[]))

choose2 :: Eq a => [a] -> [[a]]
choose2 [] = [[]]
choose2 (x:xs) = ((map (x:) (choose1 xs)) ++ choose2 xs)

