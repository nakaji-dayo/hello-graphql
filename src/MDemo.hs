{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module MDemo(schema3, serve3) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Data                  (Proxy)
import           Data.Morpheus              (App, deriveApp, interpreter,
                                             runApp)
import           Data.Morpheus.Document     (toGraphQLDocument)
import           Data.Morpheus.Types        (ComposedResolver, GQLType, QUERY,
                                             Resolver, ResolverContext (..),
                                             ResolverQ, RootResolver (..),
                                             Undefined (..), lift, liftEither,
                                             unsafeInternalContext)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text, pack, unpack)
import           Data.Traversable           (for)
import           Debug.Trace                (traceIO, traceM, traceShowM)
import qualified Entity                     as E
import qualified EntityId                   as E
import           GHC.Generics               (Generic)
-- import           Haxl                       (Haxl, MyRequest (GetBeer), getBeer,
--                                              getBeersByStore, getStore,
--                                              getStorePv, getStores, runHaxl')
import           App
import           Entity.Beer                (selectBeer)
import           Entity.Store               (selectStore)
import           Lens.Micro
import           Web.Scotty                 (body, post, raw, scotty)

newtype JinjaId = JinjaId Int
  deriving (Generic, GQLType)

data Query m = Query
  { store    :: StoreArgs -> m (Store m)
  , stores   :: m [Store m]
  , beer     :: BeerArgs -> m Beer
  , bestBeer :: m Beer
  } deriving (Generic, GQLType)

data Store m = Store
  { id    :: Text             -- Non-Nullable Field
  , name  :: Text   -- Nullable Field
  , beers :: m [Beer]
  } deriving (Generic, GQLType)

data StoreArgs = StoreArgs
  { id      :: Text        -- Required Argument
  } deriving (Generic, GQLType)

data Beer = Beer
  { id   :: Text
  , name :: Text
  , ibu  :: Int
  } deriving (Generic, GQLType)

getStore :: E.StoreId -> AppM E.Store
getStore sid = do
  debug ("getStore", sid)
  head <$> queryM selectStore sid

getBeer :: E.BeerId -> AppM E.Beer
getBeer id = do
  debug ("getBeer", id)
  head <$> queryM selectBeer id

newtype BeerArgs = BeerArgs
  { id :: Text
  } deriving (Generic, GQLType)

storeR :: StoreArgs -> ResolverQ e AppM Store
storeR StoreArgs { id } = lift $ do
    s <- getStore sid
    pure $ renderStore s
  where sid = E.StoreId $ unpack id

renderStore x = Store
  { id = x ^. #id & E.unStoreId & pack
  , name = x ^. #name & pack
  , beers = beersR (x ^. #id & E.unStoreId)
  }

beersR :: String -> ResolverQ e AppM [Beer]
beersR id = liftEither $ do
  bs <- undefined -- getBeersByStore (E.StoreId id)
  pure $ Right $ renderBeer <$> bs

renderBeer :: E.Beer -> Beer
renderBeer x = Beer
  { id = x ^. #id & E.unBeerId & pack
  , name = x ^. #name & pack
  , ibu = x ^. #ibu & fromIntegral
  }

storesR :: ComposedResolver QUERY e AppM [] Store
storesR = liftEither $ do undefined
  -- xs <- undefined -- getStores
  -- let res = fmap renderStore xs
  -- pure $ Right res

beerR :: BeerArgs -> ResolverQ e AppM Beer
beerR BeerArgs { id } = liftEither $ do
  Right . renderBeer <$> getBeer (E.BeerId $ unpack id)

bestBeerR :: ResolverQ e AppM Beer
bestBeerR = lift $ do
  let id = "8b219ec6-207e-44ef-9b82-1f403b4c7c93" -- stub
  renderBeer <$> getBeer (E.BeerId id)

queryR :: Query (Resolver QUERY e AppM)
queryR = Query
         { store = storeR
         , stores = storesR
         , beer = beerR
         , bestBeer = bestBeerR
         }

rootResolver :: RootResolver AppM () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = queryR
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

app :: App () AppM
app = deriveApp rootResolver

-- gqlApi :: ByteString -> Haxl ByteString
-- gqlApi = interpreter rootResolver

serve3 :: IO ()
serve3 = scotty 3000 $ post "/api" $ raw =<< (liftIO . runAppM . runApp app =<< body)

schema3 = BS.putStrLn $ toGraphQLDocument (Proxy :: Proxy (RootResolver IO () Query Undefined Undefined))

-- -- runHaxlApp :: MapAPI a b => App e Haxl -> a -> IO b
-- runHaxlApp haxlApp input = do
--   let stateStore = stateSet DeityState stateEmpty
--   environment <- initEnv stateStore ()
--   runHaxl environment (runApp haxlApp input)
