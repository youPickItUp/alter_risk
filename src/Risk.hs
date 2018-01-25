{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

-- | Type for representation of outcome of die roll.
newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

-- | Function enables to apply funtion to first value in pair. Returns modified pair.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- | Adjusting functions to our data type.
instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- | Function imitates die roll. Returns wrapped outcome of die roll and new generator.
die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- | Army consists of one Int - number of soldiers.
type Army = Int

-- | Battlefield consists of two armies - one attacking one defending.
data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

-- | Function executes two armies' single encounter. Returns wrapped changed Battlefield and generator.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield a d) =
  do
    let attArmy = min 3 $ max 0 (a-1)
        defArmy = min 2 d
    attRolls <- sortedRolls attArmy
    defRolls <- sortedRolls defArmy
    return $ foldl oneBattleButNotWar bf (zipWith (,) attRolls defRolls)

-- | Function imitates results of n die rolls. Returns them sorted descending as a list of wrapped DieValues.
sortedRolls :: Int -> Rand StdGen [DieValue]
sortedRolls n = reverse . sort <$> replicateM n die

-- | One step of a battle. Equivalet of one soldier death.
oneBattleButNotWar :: Battlefield -> (DieValue, DieValue) -> Battlefield
oneBattleButNotWar (Battlefield a d) (attRoll, defRoll)
  | attRoll > defRoll = Battlefield a (d - 1)
  | otherwise = Battlefield (a - 1) d

-- | Simulates battle until fewer than 2 attackers or no defenders remain.
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d)
  | a < 2 || d < 1 = return bf
  | otherwise = battle bf >>= invade

{-
attackersWin :: Battlefield -> Maybe Int
attackersWin bf@(Battlefield a d)
  | a < 2 =  Just 0
  | d < 1 =  Just 1
  | otherwise = Nothing
-}
-- / Function calculates a ratio of victories by attackers in 1000 repetitions.
successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  do
  battlefields <- replicateM 1000 (invade bf)
  let wins = foldr (\b acc -> acc + if (defenders b < 1) then 1 else 0) 0 battlefields
      battles = length battlefields
  return $ (wins / fromIntegral battles)

--showBattle :: Rand StdGen-> IO

-- | Evaluate a random computation in the IO monad.
showOutcome x = evalRandIO x
--evalRandIO()
