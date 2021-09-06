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
module MHaxl where

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
import           Haxl                       (Haxl, MyRequest (GetBeer), getBeer,
                                             getBeersByStore, getStore,
                                             getStorePv, getStores, runHaxl')
import           Lens.Micro
import           Web.Scotty                 (body, post, raw, scotty)

newtype JinjaId = JinjaId Int
  deriving (Generic, GQLType)

data Query m = Query
  { store  :: StoreArgs -> m (Store m)
  , stores :: m [Store m]
  , beer   :: BeerArgs -> m Beer
  } deriving (Generic, GQLType)

data Store m = Store
  { id    :: Text             -- Non-Nullable Field
  , name  :: Text   -- Nullable Field
  , beers :: m [Beer]
  , pv    :: m Int
  } deriving (Generic, GQLType)

data StoreArgs = StoreArgs
  { id      :: Text        -- Required Argument
  } deriving (Generic, GQLType)

data Beer = Beer
  { id   :: Text
  , name :: Text
  , ibu  :: Int
  } deriving (Generic, GQLType)

newtype BeerArgs = BeerArgs
  { id :: Text
  } deriving (Generic, GQLType)

storeR :: StoreArgs -> ResolverQ e Haxl Store
storeR StoreArgs { id } = do
  ResolverContext { currentSelection, schema, operation } <- unsafeInternalContext
  liftEither $ do
    traceShowM currentSelection
    s <- getStore sid
    pv <- getStorePv sid
    pure $ Right $ renderStore s pv
  where sid = E.StoreId $ unpack id

-- storeR :: StoreArgs -> ResolverQ e Haxl Store
-- storeR StoreArgs { id } = lift $ do
--     s <- getStore sid
--     pure $ renderStore s (storePvF sid)
--   where sid = E.StoreId $ unpack id

renderStore x pv = Store
  { id = x ^. #id & E.unStoreId & pack
  , name = x ^. #name & pack
  , beers = beersR (x ^. #id & E.unStoreId)
  , pv = pure pv}

storePvF :: E.StoreId -> ResolverQ  e Haxl Int
storePvF id = liftEither $
  Right <$> getStorePv id

beersR :: String -> ResolverQ e Haxl [Beer]
beersR id = liftEither $ do
  bs <- getBeersByStore (E.StoreId id)
  pure $ Right $ renderBeer <$> bs

renderBeer :: E.Beer -> Beer
renderBeer x = Beer
  { id = x ^. #id & E.unBeerId & pack
  , name = x ^. #name & pack
  , ibu = x ^. #ibu & fromIntegral
  }

storesR :: ComposedResolver QUERY e Haxl [] Store
storesR = liftEither $ do
  xs <- getStores
  let res = fmap (`renderStore` undefined) xs
  pure $ Right res

beerR :: BeerArgs -> ResolverQ e Haxl Beer
beerR BeerArgs { id } = liftEither $ do
  Right . renderBeer <$> getBeer (E.BeerId $ unpack id)

queryR :: Query (Resolver QUERY e Haxl)
queryR = Query
         { store = storeR
         , stores = storesR
         , beer = beerR
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

-- gqlApi :: ByteString -> Haxl ByteString
-- gqlApi = interpreter rootResolver

serve2 :: IO ()
serve2 = scotty 3000 $ post "/api" $ raw =<< (liftIO . runHaxl' . runApp app =<< body)

schema = BS.putStrLn $ toGraphQLDocument (Proxy :: Proxy (RootResolver IO () Query Undefined Undefined))

-- -- runHaxlApp :: MapAPI a b => App e Haxl -> a -> IO b
-- runHaxlApp haxlApp input = do
--   let stateStore = stateSet DeityState stateEmpty
--   environment <- initEnv stateStore ()
--   runHaxl environment (runApp haxlApp input)
