{-# LANGUAGE OverloadedStrings #-}

module Hasql.Private.Statements where

-- bytestring-tree-builder
import ByteString.TreeBuilder (toByteString)

-- hasql
import Hasql.Statement
import Hasql.Encoders
import Hasql.Decoders

-- hasql-transaction-io
import Hasql.Private.Types

startTransaction :: IsolationLevel -> Mode -> Deferrable -> Bool -> Statement () ()
startTransaction isolation mode deferrable = 
  Statement stmt noParams noResult
  where
    stmt = toByteString $
      "START TRANSACTION ISOLATION LEVEL " <>
      isolationLevelToSQL isolation <> " " <>
      modeToSQL mode <> " " <>
      deferrableToSQL deferrable

commitTransaction :: Bool -> Statement () ()
commitTransaction = Statement "COMMIT" noParams noResult

rollbackTransaction :: Bool -> Statement () ()
rollbackTransaction = Statement "ROLLBACK" noParams noResult
