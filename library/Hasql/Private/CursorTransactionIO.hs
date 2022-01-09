{-# LANGUAGE OverloadedStrings #-}

module Hasql.Private.CursorTransactionIO where

-- bytestring
import Data.ByteString (ByteString)

-- bytestring-tree-builder
import ByteString.TreeBuilder

-- transformers
import Control.Monad.Error.Class

-- mtl
import Control.Monad.Reader
import Control.Monad.State

-- unliftio-core
import Control.Monad.IO.Unlift

-- resourcet
import Control.Monad.Trans.Resource

-- hasql
import Hasql.Encoders (noParams)
import Hasql.Decoders (Result, noResult)
import Hasql.Session hiding (statement)
import Hasql.Statement

-- hasql-streaming
import Hasql.Private.Session.UnliftIO
import Hasql.TransactionIO hiding (statement)
import qualified Hasql.TransactionIO as TransactionIO

data Cursor s a = Cursor 
  { cursorVar :: ByteString
  , decoder :: Result a
  }
  deriving (Functor)

newtype CursorTransactionIO s a = CursorTransactionIO
  ( StateT Int (ResourceT TransactionIO) a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource, MonadState Int)

run :: (forall s. CursorTransactionIO s a) -> TransactionIO a
run (CursorTransactionIO ctxio) = runResourceT . flip evalStateT 0 $ ctxio

statement :: params -> Statement params result -> CursorTransactionIO s result
statement params stmt = CursorTransactionIO . lift . lift $ TransactionIO.statement params stmt

ignoreFailedTransactionError :: MonadError QueryError m => m () -> m ()
ignoreFailedTransactionError sess =
  catchError sess $ \qe -> case qe of
    QueryError _ _ (ResultError (ServerError "25P02" _ _ _)) -> pure ()
    _ -> throwError qe

declareCursorFor :: params -> Statement params result -> CursorTransactionIO s (Cursor s result)
declareCursorFor params stmt = do
  UnliftIO runInIO <- CursorTransactionIO . lift . lift $ askUnliftIO
  cursorIx <- get
  let cursorVar = toByteString $ "Hasql_CursorTransactionIO_" <> asciiIntegral cursorIx
  modify' (+1)
  (_, cursor) <- allocate
    (runInIO $ newCursor cursorVar params stmt)
    (runInIO . ignoreFailedTransactionError . closeCursor)
  pure cursor

newCursor :: ByteString -> params -> Statement params result -> TransactionIO (Cursor s result)
newCursor cursorVar params (Statement query encoder decoder prepare) = do
  let cursorQuery = 
        "DECLARE " <> cursorVar <> " NO SCROLL CURSOR FOR " <> query
  TransactionIO.statement params (Statement cursorQuery encoder noResult prepare)
  pure $ Cursor cursorVar decoder

closeCursor :: Cursor s a -> TransactionIO ()
closeCursor (Cursor cursorVar _) = do
  let closeQuery = "CLOSE " <> cursorVar
  TransactionIO.statement () (Statement closeQuery noParams noResult True)

fetchWithCursor :: Cursor s a -> CursorTransactionIO s a
fetchWithCursor (Cursor cursorVar decoder) = do
  let fetchQuery = "FETCH " <> cursorVar
  statement () (Statement fetchQuery noParams decoder True)
