{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module HaxlDemo where

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
import           EntityId                   (BeerId, StoreId)
import qualified EntityId                   as E
import           GHC.Generics               (Generic)
import           Haxl                       (Haxl, MyRequest (GetBeer), getBeer,
                                             getBeersByStore, getStore,
                                             getStorePv, getStores, runHaxl')
import           Lens.Micro
import           Web.Scotty                 (body, post, raw, scotty)
import Type

storeR :: StoreArgs -> ResolverQ e Haxl Store
storeR StoreArgs { id } =
  lift $ renderStore <$> getStore id

renderStore :: E.Store -> Store (Resolver QUERY e Haxl)
renderStore x = Store
  { id = x ^. #id
  , name = x ^. #name . to pack
  , beers = beersR $ x ^. #id
  }

beersR :: StoreId -> ResolverQ e Haxl [Beer]
beersR id =
  lift $ fmap renderBeer <$> getBeersByStore id

renderBeer :: E.Beer -> Beer
renderBeer x = Beer
  { id = x ^. #id
  , name = x ^. #name . to pack
  , ibu = x ^? #ibu . to fromIntegral
  }

storesR :: StoresArgs -> ComposedResolver QUERY e Haxl [] Store
storesR _ =
  lift $ fmap renderStore <$> getStores

beerR :: BeerArgs -> ResolverQ e Haxl Beer
beerR BeerArgs { id } = liftEither $ do
  Right . renderBeer <$> getBeer id

bestBeerR :: ResolverQ e Haxl Beer
bestBeerR = lift $ do
  let id = "b1aa264a-7062-4dce-a3c0-1353ae98f151"
  renderBeer <$> getBeer (E.BeerId id)

queryR :: Query (Resolver QUERY e Haxl)
queryR = Query
         { store = storeR
         , stores = storesR
         , beer = beerR
         , bestBeer = bestBeerR
         }

rootResolver :: RootResolver Haxl () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = queryR
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

app :: App () Haxl
app = deriveApp rootResolver

serve2 :: IO ()
serve2 = scotty 3000 $ post "/api" $ raw =<< (liftIO . runHaxl' . runApp app =<< body)

schema2 = BS.putStrLn $ toGraphQLDocument (Proxy :: Proxy (RootResolver IO () Query Undefined Undefined))
