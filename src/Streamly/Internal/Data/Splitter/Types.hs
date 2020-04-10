-- |
-- Module      : Streamly.Internal.Data.Splitter.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE ExistentialQuantification #-}

module Streamly.Internal.Data.Splitter.Types
    ( Step(..)
    , Splitter(..)
    , stepWS
    , doneWS
    , initialTS
    , initialTSM
    , split
    )where

import Streamly.Internal.Data.Strict (Tuple'(..))

data Step s b
    = Yield s
    | Stop b

data Splitter m a b =
    -- | @Splitter @ @ step @ @ initial @ @ done@
    forall s. Splitter (s -> a -> m (Step s b)) (m s) (s -> m b)

{-# INLINE stepWS #-}
stepWS :: Monad m => (s -> a -> m (Step s b)) -> Step s b -> a -> m (Step s b)
stepWS step (Yield s) a = step s a
stepWS _ x _ = return x

{-# INLINE doneWS #-}
doneWS :: Monad m => (s -> m b) -> Step s b -> m b
doneWS _ (Stop b) = return b
doneWS done (Yield s) = done s

{-# INLINE initialTS #-}
initialTS :: s -> Step s b
initialTS = Yield

{-# INLINE initialTSM #-}
initialTSM :: Monad m => m s -> m (Step s b)
initialTSM = fmap Yield

instance Monad m => Functor (Splitter m a) where
    {-# INLINE fmap #-}
    fmap f (Splitter step start done) = Splitter step' start done'
      where
        step' s a = do
            sa <- step s a
            case sa of
                Yield s1 -> return $ Yield s1
                Stop b -> return $ Stop (f b)
        done' x = fmap f $! done x

instance Monad m => Applicative (Splitter m a) where
    {-# INLINE pure #-}
    pure b = Splitter (\() _ -> pure $ Stop b) (pure ()) (\() -> pure b)
    {-# INLINE (<*>) #-}
    (Splitter stepL beginL doneL) <*> (Splitter stepR beginR doneR) =
        let combine (Stop dL) (Stop dR) = Stop $ dL dR
            combine sl sr = Yield $ Tuple' sl sr
            step (Tuple' xL xR) a =
                combine <$> stepWS stepL xL a <*> stepWS stepR xR a
            begin = Tuple' <$> initialTSM beginL <*> initialTSM beginR
            done (Tuple' xL xR) = doneWS doneL xL <*> doneWS doneR xR
         in Splitter step begin done

data SplitSeqState sb b sc
    = LeftSplitter sb
    | RightSplitter b sc

{-# INLINE split #-}
split :: Monad m => (b -> c -> d) -> Splitter m a b -> Splitter m a c -> Splitter m a d
split f (Splitter step1 initial1 done1) (Splitter step2 initial2 done2) =
    Splitter step initial done
  where
    initial = LeftSplitter <$> initial1
    step (LeftSplitter s0) a = do
        s1 <- step1 s0 a
        case s1 of
            Yield s2 -> return $ Yield $ LeftSplitter s2
            Stop b -> do
                y <- initial2
                return $ Yield $ RightSplitter b y
    step (RightSplitter b s0) a = do
        s1 <- step2 s0 a
        case s1 of
            Yield s2 -> return $ Yield $ RightSplitter b s2
            Stop c -> return $ Stop $ f b c
    done (LeftSplitter sb) = f <$> (done1 sb) <*> (initial2 >>= done2)
    done (RightSplitter b sc) = f b <$> done2 sc
