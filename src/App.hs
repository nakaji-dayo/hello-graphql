{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module App where

import           Control.Monad.Catch    (MonadCatch (catch), SomeException, onException, MonadThrow)
import           Control.Monad.IO.Class
import           Data.Pool              (Pool)
import           DataSource             (Connection, withResource, createPool')
import Control.Monad.Reader (ReaderT(ReaderT))
import Capability.Reader
import Capability.Source
import Database.HDBC (commit, IConnection (rollback))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase)
import Database.HDBC.Record (runQuery', runInsert)
import Data.UUID (UUID, toText, toString)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text, pack)
import Control.Concurrent (MVar, withMVar, newMVar)
import Text.Pretty.Simple

data Context = Context
  { pool       :: Pool Connection
  , connection :: Maybe Connection
  , lockStdIO  :: MVar ()
  }


newtype AppM a = AppM { unAppM :: Context -> IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadBase IO
    , MonadBaseControl IO
    , MonadThrow
    , MonadCatch
    ) via ReaderT Context IO
  deriving (HasReader "context" Context, HasSource "context" Context) via MonadReader (ReaderT Context IO)

type HasContext = HasReader "context" Context

withTransaction' :: (MonadIO m, MonadCatch m) => Connection  -> (Connection -> m a) -> m a
withTransaction' conn func = do
  r <- onException (func conn) doRollback
  liftIO $ commit conn
  return r
  where
    doRollback = catch (liftIO $ rollback conn) doRollbackHandler
    doRollbackHandler :: (MonadIO m, MonadCatch m) => SomeException -> m ()
    doRollbackHandler _ = return ()

withResource' :: (HasContext m, MonadBaseControl IO m) => (Connection -> m b) -> m b
withResource' op = do
  ctx <- ask @"context"
  case connection ctx of
    Just c  -> op c
    Nothing ->
      withResource (pool ctx) op

_withResourceTransaction ::
  (HasContext m, MonadBaseControl IO m, MonadIO m, MonadCatch m) =>
  (Connection -> m b) -> m b
_withResourceTransaction sub = do
  cfg <- ask @"context"
  case connection cfg of
    Just c  -> sub c
    Nothing -> withResource (pool cfg) $ \conn -> withTransaction' conn sub

runTransactionM :: (HasContext m, MonadBaseControl IO m, MonadIO m, MonadCatch m) => m a -> m a
runTransactionM operation =
  _withResourceTransaction $ \conn -> local @"context" (\c -> c { connection = Just conn }) operation

queryM r v = withResource' $ \conn -> liftIO $ runQuery' conn r v

insertM a b = _withResourceTransaction $ \conn -> liftIO $ runInsert conn a b

genId :: MonadIO m => m Text
genId = liftIO $ pack . toString <$> nextRandom

debug :: (HasContext m, MonadIO m, Show a) => a -> m ()
debug x = do
  lock <- asks @"context" lockStdIO
  liftIO $ withMVar lock $ \_ -> pPrint x

--
initialize :: IO Context
initialize = do
  pool <- createPool'
  let connection = Nothing
  lockStdIO <- newMVar ()
  pure Context {..}

runAppM :: AppM a -> IO a
runAppM x = do
  ctx <- initialize
  unAppM x ctx
