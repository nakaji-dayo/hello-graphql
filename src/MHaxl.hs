{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module MHaxl where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Data                  (Proxy)
import           Data.Morpheus              (App, deriveApp, interpreter,
                                             runApp)
import           Data.Morpheus.Document     (toGraphQLDocument)
import           Data.Morpheus.Types        (GQLType, QUERY, Resolver,
                                             ResolverQ, RootResolver (..),
                                             Undefined (..), liftEither)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Haxl                       (Haxl, runHaxl')
import           Web.Scotty                 (body, post, raw, scotty)

newtype JinjaId = JinjaId Int
  deriving (Generic, GQLType)

data Query m = Query
  { store  :: StoreArgs -> m (Store m)
  , stores :: m [Store m]
  , beer   :: BeerArgs -> m Beer
  } deriving (Generic, GQLType)

data Store m = Store
  { id   :: Text         -- Non-Nullable Field
  , name :: m (Maybe Text)   -- Nullable Field
  } deriving (Generic, GQLType)

data StoreArgs = StoreArgs
  { name      :: Text        -- Required Argument
  } deriving (Generic, GQLType)

data Beer = Beer
  { id   :: Text
  , name :: Text
  , ibu  :: Int
  } deriving (Generic, GQLType)

newtype BeerArgs = BeerArgs
  { id :: Int
  } deriving (Generic, GQLType)

deityR :: DeityArgs -> ResolverQ () Haxl Deity
deityR DeityArgs { name, mythology } = liftEither $ dbDeity name mythology

dbDeity :: forall m. (Applicative m, MonadIO m) => Text -> Maybe Text -> IO (Either String (Deity m))
dbDeity _ _ = do
  liftIO $ putStrLn "dbDeity"
  pure $ Right (Deity "susanoo" (pure $ Just "10") jinjasR)

jinjasR :: MonadIO m => m [Jinja]
jinjasR = do
  liftIO $ putStrLn "jinjasR"
  pure [Jinja 0 "yasaka" "kyoto"]

deitiesR :: (Applicative m, MonadIO m) => ResolverQ  () IO [Deity m]
deitiesR = liftEither dbDeities

dbDeities :: (Applicative m, MonadIO m) => IO (Either String [Deity m])
dbDeities = do
  liftIO $ putStrLn "dbDeities"
  pure $ Right [ Deity "susanoo" (pure $ Just "10") jinjasR -- N+1 Problem
               , Deity "tsukuyomi" (pure $ Just "9") jinjasR
               ]

jinjaR :: JinjaArgs -> ResolverQ  () IO Jinja
jinjaR _ = pure $ Jinja 1 "yasaka" "kyoto"

queryR :: Query (Resolver QUERY e Haxl)
queryR = Query
         { store = storeR
         , stores = storesR
         , beer = beersR
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

serve :: IO ()
serve = scotty 3000 $ post "/api" $ raw =<< (liftIO . runHaxl' . runApp app =<< body)

schema = BS.putStrLn $ toGraphQLDocument (Proxy :: Proxy (RootResolver IO () Query Undefined Undefined))

-- -- runHaxlApp :: MapAPI a b => App e Haxl -> a -> IO b
-- runHaxlApp haxlApp input = do
--   let stateStore = stateSet DeityState stateEmpty
--   environment <- initEnv stateStore ()
--   runHaxl environment (runApp haxlApp input)
