{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Type where

import           Entity.Beer                (selectBeer)
import           Entity.Store               (selectStore)
import           EntityId                   (BeerId, StoreId)
import           GHC.Generics               (Generic)
import Data.Morpheus.Types (GQLType)
import Data.Text (Text)

data Query m = Query
  { store    :: StoreArgs -> m (Store m)
  , stores   :: StoresArgs -> m [Store m]
  , beer     :: BeerArgs -> m Beer
  , bestBeer :: m Beer
  } deriving (Generic, GQLType)

data Store m = Store
  { id    :: StoreId
  , name  :: Text
  , beers :: m [Beer]
  } deriving (Generic, GQLType)

newtype StoreArgs = StoreArgs
  { id      :: StoreId
  } deriving (Generic, GQLType)

data StoresArgs = StoresArgs
  { name  :: Maybe Text
  , type' :: Maybe StoreType
  } deriving (Generic, GQLType)

data StoreType = LiquorStore | Pub
 deriving (Generic, GQLType)

data Beer = Beer
  { id   :: BeerId
  , name :: Text
  , ibu  :: Maybe Int
  } deriving (Generic, GQLType)

newtype BeerArgs = BeerArgs
  { id :: BeerId
  } deriving (Generic, GQLType)
