{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Haxl where

import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad
import           Data.Generics.Labels
import           Data.Hashable
import           GHC.Generics
import           Haxl.Core
import           Lens.Micro           ((^.))

htest = do
  env <- emptyEnv ()
  runHaxl env (pure 42)

data Store = Store
  { id   :: Int
  , name :: String
  } deriving (Show, Generic)

data Beer = Beer
  { id   :: Int
  , name :: String
  } deriving (Show, Generic)

data MyRequest a where
  GetStore :: Int -> MyRequest Store
  GetBeersByStore :: Int -> MyRequest [Beer]

deriving instance Eq (MyRequest a)
deriving instance Show (MyRequest a)
instance ShowP MyRequest where showp = show


instance Hashable (MyRequest a) where
  hashWithSalt s (GetStore id)        = hashWithSalt s (0 :: Int, id)
  hashWithSalt s (GetBeersByStore id) = hashWithSalt s (1 :: Int, id)

instance StateKey MyRequest where
  data State MyRequest = MyState ()


initState :: State MyRequest
initState = MyState ()

instance DataSourceName MyRequest where
  dataSourceName _ = "MyRequest"

instance DataSource u MyRequest where
  fetch = myFetch

myFetch :: State MyRequest -> Flags -> u -> PerformFetch MyRequest
myFetch (MyState _) _ _ = BackgroundFetch $ mapM_ fetchAsync



fetchAsync :: BlockedFetch MyRequest -> IO ()
fetchAsync (BlockedFetch req rvar) =
  void $ forkIO $ do
    x <- fetchData req
    putSuccess rvar x


fetchData :: MyRequest a -> IO a
fetchData (GetStore id) = do
  putStrLn "fetch store"
  threadDelay (2 * 1000 * 1000)
  pure $ Store id ("store::" <> show id)
fetchData (GetBeersByStore id) = do
  putStrLn "fetch beer"
  threadDelay (4 * 1000 * 1000)
  pure $ fmap (\id -> Beer id ("beer::" <> show id)) [0..5]


--

getStore :: Int -> GenHaxl u w Store
getStore id = dataFetch (GetStore id)

getBeersByStore :: Int -> GenHaxl u w [Beer]
getBeersByStore id = dataFetch (GetBeersByStore id)


test = do
  env <- initEnv (stateSet initState stateEmpty) ()
  runHaxl env $ do
    s0 <- getStore 0
    s1 <- getStore 1
    mapM (\x -> getBeersByStore (x ^. #id)) [s0, s1]
