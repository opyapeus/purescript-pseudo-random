module Test.Main where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Effect.Class.Console (log)
import Random.PseudoRandom (class Random, random, randomR, randomRs, randoms)
import Test.QuickCheck (Seed, mkSeed, quickCheck, (==?))

main :: Effect Unit
main = do
  log "randoms should equal to random chain"
  log "Int"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? randoms' len seed :: Array Int
  log "Number"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? randoms' len seed :: Array Number
  log "Boolean"
  quickCheck \n -> let seed = mkSeed n in randoms len seed ==? randoms' len seed :: Array Boolean
  log "randomRs should equal to randomR chain"
  log "Int"
  quickCheck \n -> let seed = mkSeed n in randomRs 0 10 len seed ==? randomRs' 0 10 len seed
  log "Number"
  quickCheck \n -> let seed = mkSeed n in randomRs 0.0 10.0 len seed ==? randomRs' 0.0 10.0 len seed
  log "Boolean"
  quickCheck \n -> let seed = mkSeed n in randomRs true false len seed ==? randomRs' true false len seed
  where len = 10

randoms' :: forall a. Random a => Int -> Seed -> Array a
randoms' n seed = map _.newVal $ iterate next n initial
  where
    initial = random seed
    next rp = random rp.newSeed

randomRs' :: forall a. Random a => a -> a -> Int -> Seed -> Array a
randomRs' min max n seed = map _.newVal $ iterate next n initial
  where
    initial = randomR min max seed
    next rp = randomR min max rp.newSeed

-- NOTE: assumption that i is greater than 0
iterate :: forall a. (a -> a) -> Int -> a -> Array a 
iterate f 0 x = []
iterate f n x = x : iterate f (n - 1) (f x)