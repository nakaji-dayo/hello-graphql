{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module MorpheusDemo(schema3, serve3) where

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
-- Import           Haxl                       (Haxl, MyRequest (GetBeer), getBeer,
--                                              getBeersByStore, getStore,
--                                              getStorePv, getStores, runHaxl')
import           App
import           Entity.Beer                (selectBeer)
import           Entity.Store               (selectStore)
import           EntityId                   (BeerId, StoreId)
import           Lens.Micro
import           Query                      (selectBeersByStoreId, selectStores)
import           Web.Scotty                 (body, post, raw, scotty)
import Type

getStore :: E.StoreId -> AppM E.Store
getStore sid = do
  debug ("[demo] getStore", sid)
  head <$> queryM selectStore sid

getStores :: AppM [E.Store]
getStores = do
  debug "[demo] getStores"
  queryM selectStores ()

getBeer :: E.BeerId -> AppM E.Beer
getBeer id = do
  debug ("[demo] getBeer", id)
  head <$> queryM selectBeer id

getBeers :: E.StoreId  -> AppM [E.Beer]
getBeers id = do
  debug ("[demo] getBeers", id)
  queryM selectBeersByStoreId  id

storeR :: StoreArgs -> ResolverQ e AppM Store
storeR StoreArgs { id } =
  lift $ renderStore <$> getStore id

renderStore :: E.Store -> Store (Resolver QUERY e AppM)
renderStore x = Store
  { id = x ^. #id
  , name = x ^. #name . to pack
  , beers = beersR $ x ^. #id
  }

beersR :: StoreId -> ResolverQ e AppM [Beer]
beersR id =
  lift $ fmap renderBeer <$> getBeers id

renderBeer :: E.Beer -> Beer
renderBeer x = Beer
  { id = x ^. #id
  , name = x ^. #name . to  pack
  , ibu = x ^? #ibu . to fromIntegral
  }

storesR :: StoresArgs -> ComposedResolver QUERY e AppM [] Store
storesR _ = lift $ fmap renderStore <$> getStores

beerR :: BeerArgs -> ResolverQ e AppM Beer
beerR BeerArgs { id } = liftEither $ do
  Right . renderBeer <$> getBeer id

bestBeerR :: ResolverQ e AppM Beer
bestBeerR = lift $ do
  let id = "b1aa264a-7062-4dce-a3c0-1353ae98f151"
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

serve3 :: IO ()
serve3 = scotty 3000 $ post "/api" $ raw =<< (liftIO . runAppM . runApp app =<< body)

schema3 = BS.putStrLn $ toGraphQLDocument (Proxy @ (RootResolver IO () Query Undefined Undefined))
