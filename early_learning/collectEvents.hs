

module MyFunctions (collectEvents) where

import Sound.Tidal.Context
import qualified Data.Set as Set

-- still not right yet

makeUnique :: Ord a => [a] -> [a]
makeUnique = Set.toList . Set.fromList

collectByTime :: Time -> [(Arc,a)] -> [a]
collectByTime onset = map snd . takeWhile ( (== onset) . fst . fst)

collectEvents :: Pattern a -> Pattern [a]
collectEvents pat =
  let
  patternAsTupleList = (patToList $ timedValues pat)
  onsets = makeUnique $ map (fst.fst) patternAsTupleList
  listOfSimultainiousEvents = map (($ patternAsTupleList).collectByTime) onsets
  in
  listToPat listOfSimultainiousEvents -- this is not the right solution yet... events still need to occur in time

_toScale :: Pattern Int -> Pattern Int -> Pattern Int
_toScale pitchSetPattern  = temporalParam toScale (collectEvents pitchSetPattern)
