{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Data.List
import Text.Regex.PCRE
import Control.Lens
import Data.Function
import Data.Maybe (mapMaybe)
import qualified Data.PSQueue as Q


type Hp = Int
type Damage = Int
type Armor = Int
type Mana = Int

data GameState = Win | Lose | InProgress Board
                    deriving (Eq, Ord, Show)

data GameStateNode = GameStateNode GameState Int deriving (Eq, Show)

data Board = Board { _player    :: Stat,
                     _boss      :: Stat,
                     _mana      :: Mana,
                     _effects   :: [Effect]
                   } deriving (Show, Eq, Ord)

data Stat = Stat { _hp :: Hp
                 , _damage :: Damage
                 , _armor :: Armor
                 } deriving (Eq, Ord, Show)

type Spell = Board -> Board

data Effect = Effect { _effectName :: String
                     , _tick       :: Board -> Board
                     , _final      :: Board -> Board
                     , _duration   :: Int                    
                     }

instance Show Effect where
    show e = (show $ _effectName e) ++ " " ++ (show $ _duration e)

instance Eq Effect where
    e == f = (_effectName e, _duration e) == (_effectName e, _duration f)

instance Ord Effect where
    compare e f = compare (_effectName e, _duration e) (_effectName f, _duration f)

instance Ord GameStateNode where
    compare (GameStateNode n _) (GameStateNode m _) = compare n m


makeLenses ''Board
makeLenses ''Stat
makeLenses ''Effect


main :: IO ()
main = do
    print solve1
    print solve2
        
solve1 :: Maybe Int
solve1 = search nextSteps Win (InProgress initBoard)

solve2 :: Maybe Int
solve2 = search nextSteps2 Win (InProgress initBoard)

nullEffect :: Effect
nullEffect = Effect {_effectName="", _tick=id, _final=id, _duration=0}


-- list of (manaCost :: Int, effectName :: String, spell :: Spell)
allSpells :: [(Int, String, Spell)]
allSpells = [ (53, "", magicMissile)
            , (73, "", drain)
            , (113, "Shield", shield)
            , (173, "Poison", poison)
            , (229, "Recharge", recharge)
            ]

magicMissile :: Spell
magicMissile = (over (boss.hp) (subtract 4)) .
               (over (mana) (subtract 53))

drain :: Spell
drain = (over (player.hp) (+2)) .
        (over (boss.hp) (subtract 2)) .
        (over (mana) (subtract 73))

shield :: Spell
shield = (over (effects) (shieldEffect:)) .
         (over (player.armor)) (+7) .
         (over (mana) (subtract 113))

poison :: Spell
poison = (over (effects) (poisonEffect:)) .
         (over (mana) (subtract 173))

recharge :: Spell
recharge = (over effects (rechargeEffect:)) .
           (over (mana) (subtract 229))

shieldEffect :: Effect
shieldEffect = Effect { _effectName = "Shield"
                      , _tick = id
                      , _final = over (player.armor) (subtract 7)
                      , _duration = 6
                      }

poisonEffect :: Effect
poisonEffect = Effect { _effectName = "Poison"
                      , _tick = over (boss.hp) (subtract 3)
                      , _final = id
                      , _duration = 6
                      }

rechargeEffect :: Effect
rechargeEffect = Effect { _effectName = "Recharge"
                        , _tick = over (mana) (+ 101)
                        , _final = id
                        , _duration = 5
                        }
bossHit :: Spell
bossHit b = over (player.hp) (subtract (max 1 ((view (boss.damage) b) - (view (player.armor) b)))) b

bleed :: Spell
bleed = over (player.hp) (subtract 1)

initBoard :: Board
initBoard = Board { _player = Stat {_hp=50, _damage=0, _armor=0}
                  , _boss = Stat {_hp=55, _damage=8, _armor=0}
                  , _mana = 500
                  , _effects = []
                  }

castSpell :: Spell -> Board -> Board
castSpell = ($)

tickEffects :: Board -> Board
tickEffects b = dropDurations $ foldr tickEffect b (b^.effects)
            where
                tickEffect :: Effect -> Board -> Board
                tickEffect e = case (e^.duration) of
                                    0         -> id
                                    1         -> (e^.final) . (e^.tick)
                                    otherwise -> e^.tick
                dropDuration e = if (e^.duration > 1) then (Just (over duration (subtract 1) e)) else Nothing
                dropDurations = over effects (mapMaybe dropDuration)

gsBind :: (Board -> GameState) -> GameState -> GameState
gsBind _ Win  = Win
gsBind _ Lose = Lose
gsBind f (InProgress b) = f b

gsMap :: (Board -> Board) -> GameState -> GameState
gsMap _ Win = Win
gsMap _ Lose = Lose
gsMap f (InProgress b) = InProgress (f b)

gsCheck :: GameState -> GameState
gsCheck Win  = Win
gsCheck Lose = Lose
gsCheck gs@(InProgress b) = if (b^.player^.hp <= 0) then Lose
                            else (if (b^.boss^.hp <= 0) then Win
                            else gs)

gameStep :: Spell -> GameState -> GameState
gameStep sp = gsCheck . (gsMap tickEffects) .  (gsMap bossHit) . gsCheck . (gsMap tickEffects) . (gsMap sp)


gameStep2 :: Spell -> GameState -> GameState
gameStep2 sp = gsCheck . (gsMap tickEffects) .  (gsMap bossHit) . gsCheck . (gsMap tickEffects) . (gsMap sp) . gsCheck . (gsMap bleed)

nextSteps :: GameState -> [(GameState, Int)]
nextSteps Win = []
nextSteps Lose = []
nextSteps gs@(InProgress b) = map (\ (manacost, _, spell) -> (gameStep spell gs, manacost)) $ filter (check b) allSpells
                    where
                        check b triple = (b^.mana >= triple^._1) && (all ((/=(triple^._2)) . (^.effectName)) (b^.effects))


nextSteps2 :: GameState -> [(GameState, Int)]
nextSteps2 Win = []
nextSteps2 Lose = []
nextSteps2 gs@(InProgress b) = map (\ (manacost, _, spell) -> (gameStep2 spell gs, manacost)) $ filter (check b) allSpells
                    where
                        check b triple = (b^.mana >= triple^._1) && (all ((/=(triple^._2)) . (^.effectName)) (b^.effects))


-- shortest path search using Dijkstra's algorithm on a directed graph generated with a neighbors function
search :: (Ord a, Eq a) => (a -> [(a,Int)]) -> a -> a -> Maybe Int
search neighbors target start = search' (Q.singleton start 0)
                where
                    search' pqueue = case (Q.minView pqueue) of
                                Nothing                      -> Nothing
                                Just (node Q.:-> d, pqueue') ->
                                    if (node == target) then (Just d) else
                                        search' (reduceKeys (map (over _2 (+d)) (neighbors node)) pqueue')

reduceKeys :: (Ord k, Ord p) => [(k,p)]-> Q.PSQ k p -> Q.PSQ k p
reduceKeys = flip (foldr (\ (a,b) -> (Q.alter (f b) a)))
                where
                    f p Nothing   = Just p
                    f p (Just p') = Just (min p p')


