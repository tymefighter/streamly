{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}

-- |
-- Module      : Streamly.Parse.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Parse.Types
    (
      Parse (..)
    , Status (..)
    , fromStatus
    )
where

import Control.Applicative (liftA2, Alternative(..))
import Streamly.Internal.Data.Strict (Tuple'(..))

-- We should incorporate this in the Fold type itself instead of having a
-- separate Parse type. We can use newtypes for different Applicative behaviors
-- of the type.
--
-- Generalizing the Fold type to incorporate Success and Failure. If the fold
-- returns a success, it means the fold result has saturated and it will not
-- changes on any further input.
{-
data Status a = Partial !a | Success !a

instance Functor Status where
    fmap f (Success a) = Success (f a)
    fmap f (Partial a) = Partial (f a)
-}
-- With the Success constructor the fold type gets generalized to "terminating
-- folds". There may be two types of terminating folds, (1) the folds succeeds
-- with the input that is given to it till now fully consumed and we can feed
-- the rest of the input to the next fold. The "take" fold comes in this
-- category, it will consume first n elements in the input and then the rest
-- can used elsewhere. "takeWhile" is another such fold, however in takewhile
-- the last step may not consume the input element, when the condition fails,
-- in that case that input element needs to be used in the remaining processing.
--
-- We can further generalize the folds to also allow for a failure case. If a
-- fold fails then the input that it consumed till now can be reused.  If the
-- fold fails we can use an Alternative composition to try another fold on the
-- same input.  This is called backtracking.
--
-- There are two possible ways to buffer the original input. Either the driver
-- can buffer the input until the fold results in a success or failure or the
-- fold accumulator itself can store the input and in case it fails the input
-- can be extracted and used by the driver to drive another fold.
--
-- If the driver buffers the input, we need a co-ordination between the fold
-- and the driver. Always succeeding folds (e.g. takeWhile) will have to
-- indicate to the driver, the point up to which that they have consumed the
-- input so that when the fold completes the driver can reuse the input from
-- that point onwards.
--
-- On the other hand, if the fold buffers the input then it can just return the
-- final result and the leftover input. The result could be a Success with the
-- leftover input or a Failure with leftover input. In case of failure, all
-- input would be the leftover input.
--
--
-- Storing the remaining input in the Fold seems simpler, so we will try that.
-- To remain fully polymorphic without a constraint, our options to store the
-- values are (1) Stream, (2) Vector. For now for simplicity we will use a
-- "SerialT Identity a" and we can later change it to a Vector. We could also
-- use an Array type but that would require a Storable constraint. Though I
-- guess most or all types that we can use in parsers would actually be
-- Storable.
--

data Status a b =
      Partial !b                -- Partial result
    | Success [a] !b            -- Unconsumed input and the result
    | Failure [a] String        -- Unconsumed input and the error

instance Functor (Status a) where
    fmap f (Partial b) = Partial (f b)
    fmap f (Success a b) = Success a (f b)
    fmap _ (Failure a e) = Failure a e

{-
instance Applicative Result where
   pure = More
   Done f <*> Done a = Done (f a)
   Done f <*> More a = More (f a)
   More f <*> Done a = More (f a)
   More f <*> More a = More (f a)
   -}

-- XXX rename to fromStatus
{-
fromResult :: Status a -> a
fromResult res =
    case res of
        Success a -> a
        Partial a -> a
-}

fromStatus :: Status a b -> b
fromStatus res =
    case res of
        Partial b -> b
        Success _ b -> b
        Failure _ _ -> error "fromStatus: failed parse"

data Parse m a b =
  -- | @Parse @ @ step @ @ initial @ @ extract@
  -- forall x. Parse (x -> a -> m (Status x)) (m (Status x)) (x -> m b)
  --
  -- XXX Perhaps change `m` to `BufferT m` where `type BufferT = StateT`?
  -- Since every Parser will have to perform buffer handeling, it is
  -- better to make it implicit.
  --
  forall x. Parse (x -> a -> m (Status a x)) (m (Status a x)) (x -> m b)

instance Monad m => Functor (Parse m a) where
    {-# INLINE fmap #-}
    fmap f (Parse step initial done) = Parse step initial done'
        where
        done' x = fmap f $! done x

    {-# INLINE (<$) #-}
    (<$) b _ = Parse (\_ _ -> return $ Success [] ())
                     (return $ Success [] ()) 
                     (\_ -> return b)

-- For folds/parses the following types of applicatives are possible:
--
-- 1) distributing the same input to all folds and then combine the results.
-- 2) Dual to stream append, each fold consumes some input and the next fold
-- takes over the rest of the input and then the applicative combines all the
-- results.
-- 3) Dual to stream interleave, divide the input among all folds in a
-- round-robin fashion.

-- | The Applicative instance of this type distributes the input to all the
-- Folds. This is a replacement of the default Applicative instance in
-- Data.Fold.Types.
--
newtype ZParse m a b = ZParse { unZParse :: Parse m a b } deriving Functor

instance Monad m => Applicative (ZParse m a) where
  {-# INLINE pure #-}
  pure b = ZParse $ pure b

  {-# INLINE (<*>) #-}
  ZParse (Parse stepL initialL doneL) <*> ZParse (Parse stepR initialR doneR) =
    ZParse (Parse step initial done)
    where
      step (Tuple' xL xR) a = do
        resL <- stepL xL a
        resR <- stepR xR a
        return $ checkRes (resL, resR)

      initial = do
        resL <- initialL
        resR <- initialR
        return $ checkRes (resL, resR)
      
      checkRes (resL, resR) = case (resL, resR) of
          (Success bL xL, Success bR xR)
            | length bL > length bR  -> Success bR (Tuple' xL xR)
            | otherwise              -> Success bL (Tuple' xL xR)
          (Success _ xL, Partial xR) -> Partial $ Tuple' xL xR
          (Partial xL, Success _ xR) -> Partial $ Tuple' xL xR
          (Partial xL, Partial xR)   -> Partial $ Tuple' xL xR
          (Failure b e, _)           -> Failure b e
          (_, Failure b e)           -> Failure b e

      done (Tuple' xL xR) =
        doneL xL <*> doneR xR



-- | The Applicative instance feeds the input to the first fold, when the first
-- fold completes the rest of the input is sent to the next fold an so on.
--
-- XXX Consider using a queue?
data ChainState a x1 f x
  -- ParserL state and all the input it consumedx
  = ParseL x1 [a]
  -- ParserL's consumed input, ParserL unconsumed input, ParserL output and 
  -- ParserR state 
  | ParseR [a] [a] f x
  -- ParserR unconsumed input, ParserL output, ParserR success state
  | ParseDone [a] f x

instance Monad m => Applicative (Parse m a) where
  {-# INLINE pure #-}
  -- XXX run the action instead of ignoring it??
  pure b = Parse (\_ _ -> pure $ Success [] ())
                 (pure $ Success [] ())
                 (\_ -> pure b)

  {-# INLINE (<*>) #-}
  (Parse stepL initialL doneL) <*> (Parse stepR initialR doneR) = 
    Parse step initial done
      where
        step (ParseL l bi) a = do
          resL <- stepL l a
          case resL of
            -- XXX Used (++) to preserve meaning, change this later
            Partial x -> return $ Partial $ ParseL x (bi ++ [a])
            Failure b e -> return $ Failure b e
            Success bL x -> do
              f <- doneL x
              resR <- initialR
              case resR of
                Success bR r -> let ai = bL ++ bR
                                in return $ Success ai $ ParseDone ai f r
                Partial r    -> return $ Partial $ ParseR bi bL f r
                Failure b e  -> return $ Failure b e

        -- XXX Used (++) to preserve meaning, change this later
        step (ParseDone b f x) a = let ai = b ++ [a]
                                   in return $ Success ai (ParseDone ai f x)

        step (ParseR li ms f x) a = go ms
          where
            go [] = do
              resR <- stepR x a
              case resR of
                Success b r -> return $ Success b $ ParseDone b f r
                Partial r   -> return $ Partial $ ParseR li [] f r
                Failure b e -> return $ Failure (li ++ b) e
            go (p:ps) = do
              resR <- stepR x p
              case resR of
                Success b r ->
                  -- XXX Used (++) to preserve meaning, change this later
                  let pb = ps ++ [a]
                      sb = b ++ pb
                  in return $ Success sb $ ParseDone sb f r
                Partial r   -> go ps
                Failure b e -> return $ Failure (li ++ b ++ ps) e

        initial = do
          resL <- initialL
          case resL of
            Partial x -> return $ Partial $ ParseL x []
            Failure b e -> return $ Failure b e
            Success bL x -> do
              f <- doneL x
              resR <- initialR
              case resR of
                Success bR r  -> let ai = bL ++ bR
                                 in return $ Success ai $ ParseDone ai f r
                Partial r     -> return $ Partial $ ParseR [] bL f r
                Failure b' e' -> return $ Failure b' e'

        done (ParseDone _ f x) = do
          r <- doneR x
          return $ f r
        done _ = error "Incomplete or failed parse"

{-
-- XXX The following causes an error as GHC is unable to infer the
-- higher rank type. The main problem is the forall quantification.
-- How to circumvent this problem?

instance Monad m => Monad (Parse m a) where
    return = pure
    Parse step initial done >>= f = Parse step' (fmap Left <$> initial) done'
        where
            step' (Left x) a = do
                r <- step x a
                case r of
                    Success x -> do
                        p@(Parse s mi d) <- f <$> done x
                        i <- mi
                        case i of
                            Success x -> return $ Success $ Right (x, p)
                            Partial x -> return $ Partial $ Right (x, p)
                    Partial x -> return $ Partial $ Left x
            step' (Right (x, p@(Parse s i d))) a = do
                r <- s x a
                case r of
                    Success x -> return $ Success $ Right (x, p)
                    Partial x -> return $ Partial $ Right (x, p)
            done' (Left _) = error "The parsing is not done yet"
            done' (Right (x, Parse _ _ d)) = d x

-}

-- There are two Alternative instances possible:
-- 1) Get first succeeding fold
-- 2) Get all succeeding folds (to get all possible parses)

-- XXX We should perhaps have just "Alt" implementation instead of
-- "Alternative".  Because we do not have a sensible identity for Alternative.

-- XXX This gets the first successful fold.
-- XXX Can be simplified?
instance Monad m => Alternative (Parse m a) where
    {-# INLINE empty #-}
    empty = Parse (\_ _ -> return $ Failure [] "Default failure")
                  (return $ Failure [] "Default failure")
                  (\_ -> error "Default failure")

    {-# INLINE (<|>) #-}
    Parse stepL initialL doneL <|> Parse stepR initialR doneR = Parse step initial done 
      where 
        step (Nothing, Nothing) _ = error "This should never occur"
        step (Just sL, Nothing) a = do
          resL <- stepL sL a
          return $ case resL of
            Partial sL'    -> Partial       (Just sL', Nothing)
            Success bL sL' -> Success bL    (Just sL', Nothing)
            Failure bL eL  -> Failure bL eL
        step (Nothing, Just sR) a = do
          resR <- stepR sR a
          return $ case resR of
            Partial sR'    -> Partial       (Nothing, Just sR')
            Success bR sR' -> Success bR    (Nothing, Just sR')
            Failure bR eR  -> Failure bR eR
        step (Just sL, Just sR) a = do
          resL <- stepL sL a
          resR <- stepR sR a
          return $ case (resL, resR) of
            (Partial sL'   , Partial sR')    -> Partial    (Just sL', Just sR')
            (Partial _     , Success bR sR') -> Success bR (Nothing , Just sR')
            (Partial sL'   , Failure _ _)    -> Partial    (Just sL', Nothing)
            (Success bL sL', Partial _)      -> Success bL (Just sL', Nothing)
            (Success bL sL', Success _ _)    -> Success bL (Just sL', Nothing)
            (Success bL sL', Failure _ _)    -> Success bL (Just sL', Nothing)
            (Failure _ _   , Partial sR')    -> Partial    (Nothing , Just sR')
            (Failure _ _   , Success bR sR') -> Success bR (Nothing , Just sR')
            -- XXX How will you combine faliures?
            -- XXX Does this affect the associativity in any way?
            (Failure bL eL, Failure _ _)     -> Failure bL eL

        initial = do
          resL <- initialL
          resR <- initialR
          -- Abstract the caseStep?
          return $ case (resL, resR) of
            (Partial sL'   , Partial sR')    -> Partial    (Just sL', Just sR')
            (Partial _     , Success bR sR') -> Success bR (Nothing , Just sR')
            (Partial sL'   , Failure _ _)    -> Partial    (Just sL', Nothing)
            (Success bL sL', Partial _)      -> Success bL (Just sL', Nothing)
            (Success bL sL', Success _ _)    -> Success bL (Just sL', Nothing)
            (Success bL sL', Failure _ _)    -> Success bL (Just sL', Nothing)
            (Failure _ _   , Partial sR')    -> Partial    (Nothing , Just sR')
            (Failure _ _   , Success bR sR') -> Success bR (Nothing , Just sR')
            -- XXX How will you combine faliures?
            -- XXX Does this affect the associativity in any way?
            (Failure bL eL , Failure _ _)    -> Failure bL eL

        done (Just sL, Nothing) = doneL sL
        done (Nothing, Just sR) = doneR sR
        done (_, _)             = error "This should never occur"

{-
-- There are two Alternative instances possible:
-- 1) Get first succeeding fold
-- 2) Get all succeeding folds (to get all possible parses)

-- XXX We should perhaps have just "Alt" implementation instead of
-- "Alternative".  Because we do not have a sensible identity for Alternative.
--
-- If the first fold fails we run the second fold.
instance MonadLazy m => Alternative (Foldr m a) where
    {-# INLINE empty #-}
    empty = Foldr (\_ _ -> fail "step empty")
                  (fail "begin empty")
                  (\_ -> fail "extract empty")

    {-# INLINE (<|>) #-}
    Foldr stepL finalL projectL <|> Foldr stepR finalR projectR = undefined
-}

{-

instance (Semigroup b, MonadLazy m) => Semigroup (Foldr m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

instance (Monoid b, MonadLazy m) => Monoid (Foldr m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

instance (MonadLazy m, Num b) => Num (Foldr m a b) where
    {-# INLINE fromInteger #-}
    fromInteger = pure . fromInteger

    {-# INLINE negate #-}
    negate = fmap negate

    {-# INLINE abs #-}
    abs = fmap abs

    {-# INLINE signum #-}
    signum = fmap signum

    {-# INLINE (+) #-}
    (+) = liftA2 (+)

    {-# INLINE (*) #-}
    (*) = liftA2 (*)

    {-# INLINE (-) #-}
    (-) = liftA2 (-)

instance (MonadLazy m, Fractional b) => Fractional (Foldr m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

instance (MonadLazy m, Floating b) => Floating (Foldr m a b) where
    {-# INLINE pi #-}
    pi = pure pi

    {-# INLINE exp #-}
    exp = fmap exp

    {-# INLINE sqrt #-}
    sqrt = fmap sqrt

    {-# INLINE log #-}
    log = fmap log

    {-# INLINE sin #-}
    sin = fmap sin

    {-# INLINE tan #-}
    tan = fmap tan

    {-# INLINE cos #-}
    cos = fmap cos

    {-# INLINE asin #-}
    asin = fmap asin

    {-# INLINE atan #-}
    atan = fmap atan

    {-# INLINE acos #-}
    acos = fmap acos

    {-# INLINE sinh #-}
    sinh = fmap sinh

    {-# INLINE tanh #-}
    tanh = fmap tanh

    {-# INLINE cosh #-}
    cosh = fmap cosh

    {-# INLINE asinh #-}
    asinh = fmap asinh

    {-# INLINE atanh #-}
    atanh = fmap atanh

    {-# INLINE acosh #-}
    acosh = fmap acosh

    {-# INLINE (**) #-}
    (**) = liftA2 (**)

    {-# INLINE logBase #-}
    logBase = liftA2 logBase
-}
