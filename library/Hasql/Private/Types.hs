{-# LANGUAGE OverloadedStrings #-}

module Hasql.Private.Types where

-- bytestring-tree-builder
import ByteString.TreeBuilder (Builder)

-- | A PostgreSQL transaction isolation level
data IsolationLevel
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Show, Eq)

isolationLevelToSQL :: IsolationLevel -> Builder
isolationLevelToSQL = \case
  ReadCommitted -> "READ COMMITTED"
  RepeatableRead -> "REPEATABLE READ"
  Serializable -> "SERIALIZABLE"

-- | A PostgreSQL transaction mode
data Mode = ReadWrite | ReadOnly
  deriving (Show, Eq)
  
modeToSQL :: Mode -> Builder
modeToSQL = \case
  ReadWrite -> "READ WRITE"
  ReadOnly -> "READ ONLY"

-- | A PostgreSQL transaction deferrability designation
data Deferrable = Deferrable | NotDeferrable
  deriving (Show, Eq)

deferrableToSQL :: Deferrable -> Builder
deferrableToSQL = \case
  Deferrable -> "DEFERRABLE"
  NotDeferrable -> "NOT DEFERRABLE"
