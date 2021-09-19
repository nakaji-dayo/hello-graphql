{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MNative where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Data                  (Proxy)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (toGraphQLDocument)
import           Data.Morpheus.Types        (GQLType, QUERY, Resolver,
                                             ResolverQ, RootResolver (..),
                                             Undefined (..), liftEither)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Web.Scotty                 (body, post, raw, scotty)

newtype JinjaId = JinjaId Int
  deriving (Generic, GQLType)

data Query m = Query
  { deity   :: DeityArgs -> m (Deity m)
  , deities :: m [Deity m]
  , jinja   :: JinjaArgs -> m Jinja
  } deriving (Generic, GQLType)

data Deity m = Deity
  { fullName :: Text         -- Non-Nullable Field
  , power    :: m (Maybe Text)   -- Nullable Field
  , jinjas   :: m [Jinja]
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic, GQLType)

data Jinja = Jinja
  { id        :: Int
  , jinjaName :: Text
  , adddress  :: Text
  } deriving (Generic, GQLType)

newtype JinjaArgs = JinjaArgs
  { jinjaId :: Int
  } deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ () IO Deity
resolveDeity DeityArgs { name, mythology } = liftEither $ dbDeity name mythology

dbDeity :: forall m. (Applicative m, MonadIO m) => Text -> Maybe Text -> IO (Either String (Deity m))
dbDeity _ _ = do
  liftIO $ putStrLn "dbDeity"
  pure $ Right (Deity "susanoo" (pure $ Just "10") jinjasR)

jinjasR :: MonadIO m => m [Jinja]
jinjasR = do
  liftIO $ putStrLn "jinjasR"
  pure [Jinja 0 "yasaka" "kyoto"]

deitiesR :: ResolverQ  () IO [Deity (Resolver QUERY () IO)]
deitiesR = liftEither dbDeities

dbDeities :: (Applicative m, MonadIO m) => IO (Either String [Deity m])
dbDeities = do
  liftIO $ putStrLn "dbDeities"
  pure $ Right [ Deity "susanoo" (pure $ Just "10") jinjasR -- N+1 Problem
               , Deity "tsukuyomi" (pure $ Just "9") jinjasR
               ]

jinjaR :: JinjaArgs -> ResolverQ  () IO Jinja
jinjaR _ = pure $ Jinja 1 "yasaka" "kyoto"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query
      { deity = resolveDeity
      , deities = deitiesR
      , jinja = jinjaR}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver

serve :: IO ()
serve = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)

schema = BS.putStrLn $ toGraphQLDocument (Proxy :: Proxy (RootResolver IO () Query Undefined Undefined))
