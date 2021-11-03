module Util where

import qualified Data.Map as M

-- |
-- map fst of tuple
-- >>> mapFst (*3) (1,2)
-- (3,2)
mapFst :: (t -> a) -> (t, b) -> (a, b)
mapFst f (a, b) = (f a, b)

-- |
-- map snd of tuple
-- >>> mapSnd (*3) (1,2)
-- (1,6)
mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (a, b) = (a, f b)

-- |
-- construct Data.Map from List.
-- the value of duplicated key is to be list.
-- >>> createMapWithListValue [(1, 2), (2, 3), (1, 4)]
-- fromList [(1,[2,4]),(2,[3])]
groupList :: Ord a => [(a, b)] -> M.Map a [b]
groupList = M.fromListWith (flip (++)) . map (mapSnd pure)
