module Hasql.Private.Session.UnliftIO where

-- mtl
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)

-- unliftio-core
import Control.Monad.IO.Unlift

-- safe-exceptions
import Control.Exception.Safe

--hasql
import Hasql.Session

instance MonadUnliftIO Session where
  withRunInIO inner = do
    conn <- ask
    res <- liftIO $ try $ inner $ \sess -> do
      run sess conn >>= either throwIO pure
    case res of
      Left e -> throwError e
      Right a -> pure a
