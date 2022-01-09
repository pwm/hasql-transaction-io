{-# LANGUAGE OverloadedStrings #-}

module Hasql.Private.Types where

-- bytestring-tree-builder
import ByteString.TreeBuilder (Builder)

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

data Mode = ReadWrite | ReadOnly
  deriving (Show, Eq)
  
modeToSQL :: Mode -> Builder
modeToSQL = \case
  ReadWrite -> "READ WRITE"
  ReadOnly -> "READ ONLY"

data Deferrable = Deferrable | NotDeferrable
  deriving (Show, Eq)

deferrableToSQL :: Deferrable -> Builder
deferrableToSQL = \case
  Deferrable -> "DEFERRABLE"
  NotDeferrable -> "NOT DEFERRABLE"
