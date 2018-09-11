module Random.PseudoRandom
    ( RandomPair
    , class Random
    , random
    , randomR
    , class Randoms
    , randoms
    , randomRs
    , class RandomEff
    , randomEff
    , randomREff
    ) where

import Prelude

import Data.Array ((:))
import Data.Int (toNumber)
import Effect (Effect)
import Random.LCG (Seed, lcgM, lcgNext, randomSeed, unSeed)

type RandomPair a =
  { newVal :: a
  , newSeed :: Seed
  }


class Ord a <= Random a where
  random :: Seed -> RandomPair a
  randomR :: a -> a -> Seed -> RandomPair a

instance randomInt :: Random Int where
  random :: Seed -> RandomPair Int
  random seed = { newVal: unSeed newSeed, newSeed: newSeed }
    where newSeed = lcgNext seed

  randomR :: Int -> Int -> Seed -> RandomPair Int
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: rp.newSeed }
      where
        rp = random seed
        newVal = rp.newVal `mod` (max - min + 1) + min

instance randomNumber :: Random Number where
  random :: Seed -> RandomPair Number
  random seed = { newVal: newVal, newSeed: intRp.newSeed }
    where
      intRp = random seed
      newVal = toNumber intRp.newVal / toNumber lcgM

  randomR :: Number -> Number -> Seed -> RandomPair Number
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: rp.newSeed }
      where
        rp = random seed
        newVal = rp.newVal * (max - min) + min

instance randomBoolean :: Random Boolean where
  random :: Seed -> RandomPair Boolean
  random seed = { newVal: newVal, newSeed: numRp.newSeed }
    where
      numRp = random seed
      newVal = numRp.newVal > 0.5

  randomR :: Boolean -> Boolean -> Seed -> RandomPair Boolean
  randomR min max seed
    | min > max = randomR max min seed -- NOTE: flip min max
    | otherwise = { newVal: newVal, newSeed: rp.newSeed }
      where
        rp = random seed
        newVal = rp.newVal && max || min


class Random a <= Randoms a where
  randoms :: Int -> Seed -> Array a
  randomRs :: a -> a -> Int -> Seed -> Array a

instance randomsRandom :: Random a => Randoms a where
  randoms :: Int -> Seed -> Array a
  randoms i = randomsF random i

  randomRs :: a -> a -> Int -> Seed -> Array a
  randomRs min max = randomsF (randomR min max)

randomsF :: forall a. Randoms a => (Seed -> RandomPair a) -> Int -> Seed -> Array a
randomsF f i seed
  | i < 0 = randomsF f (-i) seed -- NOTE: reverse i
  | i > 0 = rp.newVal : randomsF f (i - 1) rp.newSeed
    where rp = f seed
  | otherwise = [] -- i == 0


class Random a <= RandomEff a where
  -- NOTE: https://github.com/purescript/documentation/blob/master/errors/OrphanTypeDeclaration.md
  -- TODO: eliminate {}
  randomEff :: {} -> Effect a
  randomREff :: a -> a -> Effect a

instance randomEffRandom :: Random a => RandomEff a where
  randomEff :: {} -> Effect a
  randomEff _ = pure <<< _.newVal <<< random =<< randomSeed
  
  randomREff :: a -> a -> Effect a
  randomREff min max = pure <<< _.newVal <<< randomR min max =<< randomSeed
