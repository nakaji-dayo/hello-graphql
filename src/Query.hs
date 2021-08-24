{-# LANGUAGE OverloadedLabels #-}
module Query where

import           Data.Foldable                          (toList)
import           Database.Relational
import           Database.Relational.Projectable.Unsafe (OperatorContext)
import           Entity
import           EntityId


q :: Relation p r -> Query p r
q x = relationalQuery' x []

rel = q . relation
rel' = q . relation' . placeholder

selectStoreById :: Query StoreId Store
selectStoreById = rel' $ \ph -> do
  s <- query store
  wheres $ s ! #id .=. ph
  pure s

selectBeersByStoreId :: Query StoreId Beer
selectBeersByStoreId = rel' $ \ph -> do
  sb <- query storeBeer
  b <- query beer
  on $ sb ! #beerId .=. b ! #id
  wheres $ sb ! #storeId .=. ph
  pure b


-- valuesDef ::
--   (LiteralSQL t, OperatorContext c, Foldable f) =>
--   t -> f t -> RecordList (Record c) t
-- valuesDef def xs
--   | null xs = values [def]
--   | otherwise = values (toList xs)

-- values' ::
--   (LiteralSQL t,
--    OperatorContext c, Foldable f) =>
--   f t -> RecordList (Record c) t
-- values' = valuesDef ""

selectBeersInSore :: [StoreId] -> Query () (StoreId, Beer)
selectBeersInSore ids = rel $ do
  sb <- query storeBeer
  b <- query beer
  on $ sb ! #beerId .=. b ! #id
  wheres $ sb ! #storeId `in'` values ids
  pure $ sb ! #storeId >< b
