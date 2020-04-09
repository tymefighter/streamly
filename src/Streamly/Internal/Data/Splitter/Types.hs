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
    , mapStep
    , combineStep2
    , split
    )where

import Streamly.Internal.Data.Strict (Tuple'(..))

data Step s
  = Yield s
  | Stop s

data Splitter m a b =
  -- | @Splitter @ @ step @ @ initial @ @ done@
  forall s. Splitter (s -> a -> m (Step s)) (m s) (s -> m b)

{-# INLINE stepWS #-}
stepWS :: Monad m => (s -> a -> m (Step s)) -> Step s -> a -> m (Step s)
stepWS _ x@(Stop _) _ = return x
stepWS step (Yield s) a = step s a

{-# INLINE doneWS #-}
doneWS :: (s -> m b) -> Step s -> m b
doneWS done (Stop s) = done s
doneWS done (Yield s) = done s

{-# INLINE initialTS #-}
initialTS :: s -> Step s
initialTS = Yield

{-# INLINE initialTSM #-}
initialTSM :: Monad m => m s -> m (Step s)
initialTSM = fmap Yield

{-# INLINE mapStep #-}
mapStep :: (a -> b) -> Step a -> Step b
mapStep f (Yield a) = Yield (f a)
mapStep f (Stop a) = Stop (f a)

{-# INLINE combineStep2 #-}
combineStep2 :: (Step a -> Step b -> c) -> (Step a) -> (Step b) -> Step c
combineStep2 f sa@(Stop _) sb@(Stop _) = Stop (f sa sb)
combineStep2 f sa sb = Yield (f sa sb)

instance Monad m => Functor (Splitter m a) where
  {-# INLINE fmap #-}
  fmap f (Splitter step start done) = Splitter step start done'
    where
      done' x = fmap f $! done x

instance Monad m => Applicative (Splitter m a) where
  {-# INLINE pure #-}
  pure b = Splitter (\() _ -> pure $ Stop ()) (pure ()) (\() -> pure b)
  {-# INLINE (<*>) #-}
  (Splitter stepL beginL doneL) <*> (Splitter stepR beginR doneR) =
    let step (Tuple' xL xR) a =
          combineStep2 Tuple' <$> stepWS stepL xL a <*> stepWS stepR xR a
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
            Stop s2 -> do
                x <- done1 s2
                y <- initial2
                return $ Yield $ RightSplitter x y
    step (RightSplitter b s) a = mapStep (RightSplitter b) <$> step2 s a
    done (LeftSplitter sb) = f <$> (done1 sb) <*> (initial2 >>= done2)
    done (RightSplitter b sc) = f b <$> done2 sc
