module Hasql.CursorTransactionIO.TransactionIO (
  cursorTransactionIO,
) where

import Hasql.Private.TransactionIO (TransactionIO)
import Hasql.Private.CursorTransactionIO

-- | Run a `CursorTransactionIO` to produce a `TransactionIO` that manages the lifespan of all allocated `Cursor`s
cursorTransactionIO :: (forall s. CursorTransactionIO s a) -> TransactionIO a
cursorTransactionIO = run
