{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

#include "inline.h"

-- |
-- Module      : Streamly.Streams.SVar
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.SVar
    (
      fromSVar
    , toSVar
    , maxThreads
    , maxBuffer
    , maxYields
    )
where

import Control.Monad.Catch (throwM)

import Streamly.SVar
import Streamly.Streams.StreamK
import Streamly.Streams.Serial (SerialT)

-- MVar diagnostics has some overhead - around 5% on asyncly null benchmark, we
-- can keep it on in production to debug problems quickly if and when they
-- happen, but it may result in unexpected output when threads are left hanging
-- until they are GCed because the consumer went away.

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar Stream m a -> Stream m a
fromStreamVar sv = Stream $ \st stp sng yld -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    unStream (processEvents $ reverse list) (rstState st) stp sng yld

    where

    allDone stp = do
#ifdef DIAGNOSTICS
#ifdef DIAGNOSTICS_VERBOSE
            svInfo <- liftIO $ dumpSVar sv
            liftIO $ hPutStrLn stderr $ "fromStreamVar done\n" ++ svInfo
#endif
#endif
            stp

    {-# INLINE processEvents #-}
    processEvents [] = Stream $ \st stp sng yld -> do
        done <- postProcess sv
        if done
        then allDone stp
        else unStream (fromStreamVar sv) (rstState st) stp sng yld

    processEvents (ev : es) = Stream $ \st stp sng yld -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> unStream rest (rstState st) stp sng yld
                    Just ex -> throwM ex

{-# INLINE fromSVar #-}
fromSVar :: (MonadAsync m, IsStream t) => SVar Stream m a -> t m a
fromSVar sv = fromStream $ fromStreamVar sv

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toSVar :: (IsStream t, MonadAsync m) => SVar Stream m a -> t m a -> m ()
toSVar sv m = toStreamVar sv (toStream m)

-------------------------------------------------------------------------------
-- Concurrency control
-------------------------------------------------------------------------------
--
-- | Specify the maximum number of threads that can be spawned concurrently
-- when using concurrent streams. This is not the grand total number of threads
-- but maximum threads at each point of concurrency.
-- A value of 0 resets the thread limit to default, a negative value means
-- there is no limit. The default value is 1500.
--
-- @since 0.4.0
{-# INLINE_NORMAL maxThreads #-}
maxThreads :: IsStream t => Int -> t m a -> t m a
maxThreads n m = fromStream $ Stream $ \st stp sng yld -> do
    let n' = if n == 0 then defaultMaxThreads else n
    unStream (toStream m) (st {threadsHigh = n'}) stp sng yld

{-# RULES "maxThreadsSerial serial" maxThreads = maxThreadsSerial #-}
maxThreadsSerial :: Int -> SerialT m a -> SerialT m a
maxThreadsSerial _ = id

-- | Specify the maximum size of the buffer for storing the results from
-- concurrent computations. If the buffer becomes full we stop spawning more
-- concurrent tasks until there is space in the buffer.
-- A value of 0 resets the buffer size to default, a negative value means
-- there is no limit. The default value is 1500.
--
-- @since 0.4.0
{-# INLINE_NORMAL maxBuffer #-}
maxBuffer :: IsStream t => Int -> t m a -> t m a
maxBuffer n m = fromStream $ Stream $ \st stp sng yld -> do
    let n' = if n == 0 then defaultMaxBuffer else n
    unStream (toStream m) (st {bufferHigh = n'}) stp sng yld

{-# RULES "maxBuffer serial" maxBuffer = maxBufferSerial #-}
maxBufferSerial :: Int -> SerialT m a -> SerialT m a
maxBufferSerial _ = id

-- Stop concurrent dispatches after this limit. This is useful in API's like
-- "take" where we want to dispatch only upto the number of elements "take"
-- needs.  This value applies only to the immediate next level and is not
-- inherited by everything in enclosed scope.
{-# INLINE_NORMAL maxYields #-}
maxYields :: IsStream t => Maybe Int -> t m a -> t m a
maxYields n m = fromStream $ Stream $ \st stp sng yld -> do
    unStream (toStream m) (st {yieldLimit = n}) stp sng yld

{-# RULES "maxYields serial" maxYields = maxYieldsSerial #-}
maxYieldsSerial :: Maybe Int -> SerialT m a -> SerialT m a
maxYieldsSerial _ = id
