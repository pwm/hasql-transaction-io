module Hasql.TransactionIO.Sessions (
  transactionIO,
  unpreparedTransactionIO,
  IsolationLevel(..),
  Mode(..),
  Deferrable(..),
) where

-- hasql
import Hasql.Session (Session)

-- hasql-streaming
import Hasql.Private.TransactionIO
import Hasql.Private.Types

-- | Run a prepared transaction with the given properties
transactionIO :: IsolationLevel -> Mode -> Deferrable -> TransactionIO a -> Session a
transactionIO isolation mode deferrable txio = run txio isolation mode deferrable True

-- | Run an unprepared transaction with the given properties
unpreparedTransactionIO :: IsolationLevel -> Mode -> Deferrable -> TransactionIO a -> Session a
unpreparedTransactionIO isolation mode deferrable txio = run txio isolation mode deferrable False
