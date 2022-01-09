module Hasql.CursorTransactionIO.TransactionIO (
  cursorTransactionIO,
) where

import Hasql.Private.TransactionIO (TransactionIO)
import Hasql.Private.CursorTransactionIO

cursorTransactionIO :: (forall s. CursorTransactionIO s a) -> TransactionIO a
cursorTransactionIO = run
