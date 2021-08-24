{-# LANGUAGE FlexibleContexts #-}
module Seed where

import           App
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Entity
import           Entity.Beer
import           Entity.Store
import           Entity.StoreBeer
import           EntityId

seed :: (MonadIO m, HasContext m, MonadBaseControl IO m, MonadCatch m) => m StoreId
seed = do

  sid <- StoreId <$> genId
  insertM insertStore $ Store sid "storeX"

  bids <- forM_ [0..5] $ \i -> do
    bid <- BeerId <$> genId
    let b = Beer bid ("fuga:" <> show i) 20
    insertM insertBeer b
    sbid <- StoreBeerId <$> genId
    insertM insertStoreBeer $ StoreBeer sbid sid bid

  pure sid
