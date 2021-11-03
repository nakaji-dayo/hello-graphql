{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Haxl where

import           App
import           Control.Concurrent     (forkIO)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (foldl')
import           Data.Generics.Labels
import           Data.Hashable
import qualified Data.Map               as M
import           Entity
import           Entity.Beer            (selectBeer)
import           Entity.Store           (selectStore)
import           EntityId
import           GHC.Generics
import           Haxl.Core
import           Lens.Micro
import           Prelude
import           Query                  (selectBeersByStoreId,
                                         selectBeersInStores, selectStores)
import           Text.Pretty.Simple     (pPrint)
import           Util                   (groupList)

data MyRequest a where
  GetStore :: StoreId -> MyRequest Store
  GetStores :: MyRequest [Store]
  GetBeersByStore :: StoreId -> MyRequest [Beer]
  GetBeer :: BeerId -> MyRequest Beer
  GetStorePv :: StoreId -> MyRequest Int

deriving instance Eq (MyRequest a)
deriving instance Show (MyRequest a)
instance ShowP MyRequest where showp = show

instance Hashable (MyRequest a) where
  hashWithSalt s (GetStore (StoreId id))        = hashWithSalt s (0 :: Int, id)
  hashWithSalt s (GetBeersByStore (StoreId id)) = hashWithSalt s (1 :: Int, id)
  hashWithSalt s GetStores = hashWithSalt s (2 :: Int)
  hashWithSalt s (GetBeer (BeerId id)) = hashWithSalt s (3 :: Int, id)
  hashWithSalt s (GetStorePv (StoreId id)) = hashWithSalt s (4 :: Int, id)

instance StateKey MyRequest where
  data State MyRequest = MyState ()

initState :: State MyRequest
initState = MyState ()

instance DataSourceName MyRequest where
  dataSourceName _ = "MyRequest"

instance DataSource Context MyRequest where
  fetch _ _ ctx = BackgroundFetch (fetcher ctx)

fetcher ctx fetches = do
  mapM_ (fetchAsync ctx) otherFetches
  unless (null beerFetches) $
    flip unAppM ctx $ fetchBeers beerFetches
  where
    (beerFetches, otherFetches) = foldl' f ([], []) fetches
    f :: ([(StoreId, ResultVar [Beer])], [BlockedFetch MyRequest]) -> BlockedFetch MyRequest ->  ([(StoreId, ResultVar [Beer])], [BlockedFetch MyRequest])
    f acc (BlockedFetch (GetBeersByStore id) r) =
      acc & _1 %~ ((id, r):)
    f acc req =
      acc & _2 %~ (req:)

fetchBeers :: [(StoreId, ResultVar [Beer])] -> AppM ()
fetchBeers fetches = do
  debug ("fetchBeers: store_id in", ids)
  sbs <- M.assocs . groupList <$> queryM (selectBeersInStores ids) ()
  forM_ sbs $ \(sid, beers) -> case lookup sid fetches of
    Just r -> liftIO $ putSuccess r beers
  where ids = fst <$> fetches

fetchAsync :: Context -> BlockedFetch MyRequest -> IO ()
fetchAsync ctx (BlockedFetch req rvar) =
  void $ forkIO $ flip unAppM ctx $ do
    x <- fetchData req
    liftIO $ putSuccess rvar x

fetchData :: MyRequest a -> AppM a
fetchData (GetStore id) = do
  debug "fetch store"
  head <$> queryM selectStore id
fetchData (GetBeersByStore id) = do
  debug "fetch beer"
  queryM selectBeersByStoreId id
fetchData GetStores = do
  debug "fetch stores"
  queryM selectStores ()
fetchData (GetBeer id) = do
  debug ("fetch beer", id)
  head <$> queryM selectBeer id
fetchData (GetStorePv id) = do
  debug "fetch store pv"
  pure 10000

getStore :: StoreId -> Haxl Store
getStore id = dataFetch (GetStore id)

getBeersByStore :: StoreId -> Haxl [Beer]
getBeersByStore id = dataFetch (GetBeersByStore id)

getStores :: Haxl [Store]
getStores = dataFetch GetStores

getBeer :: BeerId -> Haxl Beer
getBeer = dataFetch . GetBeer

getStorePv :: StoreId -> Haxl Int
getStorePv = dataFetch  . GetStorePv

type Haxl = GenHaxl Context ()

runHaxl' m = do
  print "runHaxl'"
  ctx <- initialize
  env <- initEnv (stateSet initState stateEmpty) ctx
  runHaxl env m
