-- |
-- Module      : Streamly.Internal.Data.Splitter
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Internal.Data.Splitter
    ( Step(..)
    , Splitter(..)
    , any
    , all
    )where

import Streamly.Internal.Data.Splitter.Types
import Prelude hiding (any, all)

{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Splitter m a Bool
any predicate = Splitter step initial return
    where
    initial = return False
    step s a = return $
        if s
        then Stop True
        else
            if predicate a
            then Stop True
            else Yield False

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Splitter m a Bool
all predicate = Splitter step initial return
    where
    initial = return True
    step s a = return $
        if s
        then
            if predicate a
            then Yield True
            else Stop False
        else Stop False
