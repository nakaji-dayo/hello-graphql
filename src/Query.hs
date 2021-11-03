{-# LANGUAGE OverloadedLabels #-}
module Query where

import           Data.Foldable                          (toList)
import           Database.Relational
import           Database.Relational.Projectable.Unsafe (OperatorContext)
import           Entity
import qualified Entity.Beer                            as Beer
import           EntityId


q :: Relation p r -> Query p r
q x = relationalQuery' x []

rel = q . relation
rel' = q . relation' . placeholder

selectStores :: Query () Store
selectStores = rel $ query store

selectBeersByStoreId :: Query StoreId Beer
selectBeersByStoreId = rel' $ \ph -> do
  sb <- query storeBeer
  b <- query beer
  on $ sb ! #beerId .=. b ! #id
  wheres $ sb ! #storeId .=. ph
  pure b

selectBeersInStores :: [StoreId] -> Query () (StoreId, Beer)
selectBeersInStores ids = rel $ do
  sb <- query storeBeer
  b <- query beer
  on $ sb ! #beerId .=. b ! #id
  wheres $ sb ! #storeId `in'` values ids
  pure $ sb ! #storeId >< b
