{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE StrictData #-}

module Knapsack where

import Data.Ord
import Control.Monad.ST
import Control.Monad
import Data.Ix
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Algorithms.Intro as Intro
import Debug.Trace

data PackInstance = PackInstance
  { itemCount :: Int
  , values :: V.Vector Int
  , capacity :: Int
  , weights :: V.Vector Int
  } deriving Show

data PackSolution = PackSolution
  { totalValue :: Int
  , bestItems :: V.Vector Int
  , totalWeight :: Int
  } deriving Show

-- | Pack/Select items from an input set of items
-- such that the total weight of the packed items stays within
-- the specified `capacity`. The items in the implicit
-- input set are  numbered and indexed in a range
-- between 0 (included) and the specified `itemCount` (excluded).
-- Each such item has a weight and a value under
-- the same index in the specified `values` and `weights` vector.
-- Returns a vector of indexes of packed items, together with their
-- total value and their total weight.
-- The behavior is undefined unless `1 <= itemCount`,
-- `length values == length weights == itemCount,
-- every value v in `values` satisfies `0 <= v`,
-- and every weight w in `weights` satisfies  `0 <= w <= capacity`.
-- Note that there is no guarantee of optimality here, but it
-- has a runtime complexity of O(n) == O(itemCount)
-- and performans well on most instances.
packGreedy :: PackInstance -> PackSolution
packGreedy packInstance
  =  PackSolution totalValue bestItems totalWeight
  where
    -- shortcuts necessary to make math operations succint and assessable
    n = itemCount packInstance
    vs = values packInstance
    c = capacity packInstance
    ws = weights packInstance
    is = V.enumFromN 0 n -- ^ item numbers [0,1,2..n-1]
    -- | Effiency rates: Value per unit of weight.
    effs = V.zipWith (\v w -> iToD v / iToNonZeroD w) vs ws
    -- | Sorted item numbers by efficiency; best item is moved to index 0
    sortIs = V.modify (Intro.sortBy (comparing (Down . (effs V.!)))) is
    -- | Go trough efficiency-sorted items and pack as long as items fit
    (bestItems', remainingCapacity, totalValue)
      = V.foldl' (\(is, remC, totV) i ->
        let w = ws V.! i
            v = vs V.! i
        in if w > remC
          then (is, remC, totV) -- skip
          else (i:is, remC-w, totV+v) -- pack
        ) ([], c, 0) sortIs
    bestItems = V.fromList bestItems' -- convert from list to vector
    totalWeight = c - remainingCapacity

-- See 'packGreedy'
-- Note This is an alternative Greedy implementation
-- with a guarantee that the value v of a solution
-- satisfies 0.5*opt <= v <= opt,
-- where opt is the optimum.
-- On most instances this method performs worse than
-- 'packGreedy' !
packGreedyRedux :: PackInstance -> PackSolution
packGreedyRedux packInstance
  =  PackSolution totalValue' bestItems totalWeight
  where
    -- shortcuts necessary to make math operations succint and assessable
    n = itemCount packInstance
    vs = values packInstance
    c = capacity packInstance
    ws = weights packInstance
    is = V.enumFromN 0 n -- ^ item numbers [0,1,2..n-1]
    -- | Effiency rates: Value per unit of weight.
    effs = V.zipWith (\v w -> iToD v / iToNonZeroD w) vs ws
    -- | Sorted item numbers, values and weights by item
    -- efficiency; best item is moved to index 0
    sortIs = V.modify (Intro.sortBy (comparing (Down . (effs V.!)))) is
    sortVs = V.backpermute vs sortIs
    sortWs = V.backpermute ws sortIs
    cumSortWs = V.postscanl' (+) 0 sortWs
    -- bestItems' = V.takeWhile (\i -> traceShow i $ (cumSortWs V.! i) <= c) sortIs

    -- Create potential bestItems by packing until a first item doesn't fit
    bestItems'Ws = V.takeWhile (<=c) cumSortWs
    bestItems' = V.take (V.length bestItems'Ws) sortIs
    bestItems'V = V.sum $ V.take (V.length bestItems') sortVs
    bestItems'W = V.last bestItems'Ws
    -- Get the first item that didn't fit anymore
    fstNonFit = sortIs V.! (min (n-1) (V.length bestItems'))
    -- Replace all those previous items with that single item
    -- if that item alone is better; this potential swap gives the bounded
    -- optimality guarantee.
    fstNonFitV = vs V.! fstNonFit
    fstNonFitW = ws V.! fstNonFit
    (bestItems_, totalValue', totalWeight) =
      if fstNonFitW <= c && fstNonFitV >= bestItems'V
      then (V.singleton fstNonFit, fstNonFitV, fstNonFitW)
      else (bestItems', bestItems'V, bestItems'W)
    -- opt = show $ totalValue $ packOptimal packInstance -- debug
    -- greed = show $ totalValue $ packGreedy packInstance
    -- str = "Opt: " ++ opt ++ " Greed: " ++ greed ++ " Greed2: " ++ show totalValue' ++ "Greed2prev :" ++ show bestItems'V ++ " " ++ show cumSortWs
    -- bestItems = trace str bestItems_
    bestItems = bestItems_


-- See 'packGreedy'
-- Note that it uses dynamic programming, has
-- a guarantee of optimality and a runtime
-- complexity of O(n * c) == O(itemCount * capacity).
packOptimal :: PackInstance -> PackSolution
packOptimal packInstance
  =  PackSolution totalValue bestItems totalWeight
  where
    -- shortcuts necessary to make math operations succint and assessable
    n = itemCount packInstance
    vs = values packInstance
    cMax = capacity packInstance
    ws = weights packInstance
    w_i0 = ws V.! 0
    v_i0 = vs V.! 0
    -- Incrementally construct a lookup table `m`.
    -- Each slot m[i,c] contains a pair (v,b)
    -- with the highest achievable total value v
    -- when using only items 0 to i with a capacity limit c,
    -- The boolean flag 'b' stores whether
    -- item i is part of an optimal solution.

    -- Enable use of convenient 2D indexes (i,c) with 1D indexed table 'm'
    ixRange = ((0,0),(n-1, cMax)) -- Represents [(0,0), (0,1) .. (n-1, cMax)]
    ix = index ixRange -- 2D-to-1D index conversion function
    tableSize = rangeSize ixRange
    m :: (V.Vector (Int,Bool))
    m = runST $ do -- Workaround to use mutable vectors in Haskell
      -- Create a mutable vector, necessary for performance
      m <- VM.new tableSize :: ST s (VM.MVector s (Int,Bool))
      let get (i,c) = VM.read m $ ix (i,c) -- convenient getter/setters
      let set (i,c) value = VM.write m (ix (i,c)) value
      -- Base case: First item
      -- Max-achievable value with only item 0 is either 0 if doesn't fit
      -- or its value v_i0 if it fits.
      forM_ [0 .. (min cMax (w_i0-1))] $
        \c -> set (0,c) $ (0, skip_i) -- doesn't fit
      forM_ [w_i0 .. cMax] $
        \c -> set (0,c) $ (v_i0, take_i) -- fits
      -- Remaining cases/items:
      forM_ (range ((1,0),(n-1,cMax))) $ \(i,c) -> do
        -- Max-achievable value using only items 0 to i-1 and a capacity c
        (maxPrevVwithC,_) <- get (i-1, c)
        let w_i = ws V.! i
        if w_i > c
        then set (i,c) (maxPrevVwithC, skip_i)
        else do
          -- Max-achievable value with i-1 items but capacity c reduced
          -- by item weight w_i
          (maxPrevVwithRedC,_) <- get (i-1, c-w_i)
          let v_i = vs V.! i
          let maxVwithC = maxPrevVwithRedC + v_i
          if maxVwithC > maxPrevVwithC
          then set (i,c) (maxVwithC, take_i)
          else set (i,c) (maxPrevVwithC, skip_i)
      V.unsafeFreeze m -- Make 'm' immutable
    -- Max-achievable value using all items == m[n-1,cMax]
    (totalValue,_) = V.last m
    -- Backtrack through table 'm' to collect all items that need to be packed
    (bestItems', remainingCapacity) =
      foldl' (\(is, remC) i->
      let (_, shouldPackItem) = m V.! (ix (i,remC))
          w_i = ws V.! i
      in if shouldPackItem
          then (i:is, remC-w_i)
          else (is, remC)
        ) ([],cMax) [n-1,n-2..0]
    totalWeight = cMax - remainingCapacity
    bestItems = V.fromList bestItems'

take_i = True -- more expressive flag descriptions
skip_i = False


-- See 'packGreedy'
-- Note that there is a guarantee that the value v of a solution
-- satisfies (1-epsilon)*opt <= v <= opt,
-- where opt is the best possible, optimal value.
-- Thus, the specified 'epsilon' is a factor of how much a solution
-- is allowed to deviate from the optimum,
-- i.e. epsilon == 0.1 represents a maximal 10% deviation.
packFPTAS :: DeviationFactor -> PackInstance -> PackSolution
packFPTAS epsilon packInstance
  =  result { totalValue = realTotalValue}
  where
    n = itemCount packInstance
    vs = values packInstance
    vMax = V.maximum (values packInstance)
    -- Depending on epsilon simplify this instance by truncating/
    -- cutting off some least significant digits of values in 'values'
    k = epsilon * (iToD vMax / iToD n) -- scaling factor
    cutVs = V.map (\v -> floor $ iToD v / k) vs
    simplified = packInstance {values = cutVs}
    result = packOptimal simplified
    -- 'result' is approximately optimal within the 'epsilon' bound
    -- but its totalValue is wrong since it summed up truncated values
    realTotalValue = V.sum $ V.backpermute vs $ bestItems result
type DeviationFactor = Double

iToD = fromIntegral :: Int -> Double -- usefull conversion functions
iToNonZeroD :: Int -> Double
iToNonZeroD x = if x == 0 then 0.000001 else iToD x
