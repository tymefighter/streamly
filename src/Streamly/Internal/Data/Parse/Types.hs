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
    , fromResult
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

data Status a = Partial !a | Success !a

instance Functor Status where
    fmap f (Success a) = Success (f a)
    fmap f (Partial a) = Partial (f a)

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
{-
data Status a b =
      Partial !b                      -- partial result
    | Success (Array a) !b            -- Unconsumed input and the result
    | Failure (Array a) (Array Char)  -- Unconsumed input and the error

instance Functor (Status a) where
    fmap f (Partial b) = Partial (f b)
    fmap f (Success a b) = Success a (f b)
    fmap _ (Failure a e) = Failure a e
-}

{-
instance Applicative Result where
   pure = More
   Done f <*> Done a = Done (f a)
   Done f <*> More a = More (f a)
   More f <*> Done a = More (f a)
   More f <*> More a = More (f a)
   -}

-- XXX rename to fromStatus
fromResult :: Status a -> a
fromResult res =
    case res of
        Success a -> a
        Partial a -> a

{-
-- XXX rename to fromStatus
fromSuccess :: Status a b -> b
fromSuccess res =
    case res of
        Partial b -> b
        Success _ b -> b
        Failure _ _ -> error "fromSuccess: failed parse"
-}

data Parse m a b =
  -- | @Parse @ @ step @ @ initial @ @ extract@
  forall x. Parse (x -> a -> m (Status x)) (m (Status x)) (x -> m b)
  -- forall x. Parse (x -> a -> m (Status a x)) (m (Status a x)) (x -> m b)

instance Monad m => Functor (Parse m a) where
    {-# INLINE fmap #-}
    fmap f (Parse step initial done) = Parse step initial done'
        where
        done' x = fmap f $! done x

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

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
        let step x@(Tuple' xL xR) a =
                    -- XXX we can keep xL and xR without the Result wrapper
                    case xL of
                        Success _ ->
                            case xR of
                                -- XXX ideally it should not occur, as the fold
                                -- driver should not be driving the fold after
                                -- it has already returned Success previously.
                                -- XXX Replace this with an informative "error" ?
                                Success _ -> return (Success x)
                                Partial r -> do
                                    resR <- stepR r a
                                    return $ case resR of
                                        Success _ -> Success $ Tuple' xL resR
                                        Partial _ -> Partial $ Tuple' xL resR
                        Partial l ->
                            case xR of
                                Success _ -> do
                                    resL <- stepL l a
                                    return $ case resL of
                                        Success _ -> Success $ Tuple' resL xR
                                        Partial _ -> Partial $ Tuple' resL xR
                                Partial r -> do
                                    resL <- stepL l a
                                    resR <- stepR r a
                                    return $ case (resL, resR) of
                                        (Success _, Success _) -> Success $ Tuple' resL resR
                                        (_, _)           -> Partial $ Tuple' resL resR

            initial = do
                resL <- initialL
                resR <- initialR
                return $ case (resL, resR) of
                    (Success _, Success _) -> Success $ Tuple' resL resR
                    (_, _)           -> Partial $ Tuple' resL resR

            done (Tuple' xL xR) =
                doneL (fromResult xL) <*> doneR (fromResult xR)

        in  ZParse (Parse step initial done)

-- | The Applicative instance feeds the input to the first fold, when the first
-- fold completes the rest of the input is sent to the next fold an so on.
--
data ChainState x1 f x = ParseL x1 | ParseR f x | ParseDone f x

instance Monad m => Applicative (Parse m a) where
    {-# INLINE pure #-}
    -- XXX run the action instead of ignoring it??
    pure b = Parse (\_ _ -> pure $ Success ()) (pure $ Success ()) (\_ -> pure b)

    {-# INLINE (<*>) #-}
    (Parse stepL initialL doneL) <*> (Parse stepR initialR doneR) =
        -- XXX Return informative error on consuming when ParseDone?
        let step (ParseDone f x) _ = return $ Success (ParseDone f x)
            step (ParseR f x) a = do
                resR <- stepR x a
                case resR of
                    Success r -> return $ Success $ ParseDone f r
                    Partial r -> return $ Partial $ ParseR f r

            step b@(ParseL f) a = do
                    resL <- stepL f a
                    case resL of
                        Success x -> do
                            f <- doneL x
                            resR <- initialR
                            case resR of
                                Success r -> return $ Success $ ParseDone f r
                                Partial r -> return $ Partial $ ParseR f r
                        Partial x -> return $ Partial $ ParseL x

            initial = do
                resL <- initialL
                case resL of
                    Success x -> do
                        f <- doneL x
                        resR <- initialR
                        case resR of
                            Success r -> return $ Success $ ParseDone f r
                            Partial r -> return $ Partial $ ParseR f r
                    Partial x -> return $ Partial (ParseL x)

            done (ParseDone f x) = do
                r <- doneR x
                return $ f r
            done _ = error "incomplete or failed parse"
        in Parse step initial done

{-

-- XXX The following causes an error as GHC is unable to infer the
-- higher rank type. The main problem is the forall quantification.
-- One solution is to have two types: IParse and Parse
-- IParse x m a b = IParse (x -> a -> m (Status x)) (m (Status x)) (x -> m b) 
-- Parse m a b = forall x. Parse (IParse x m a b) deriving (..)

instance Monad m => Monad (Parse m a) where
    return = pure
    Parse step initial done >>= f =
        let step' (Left x) a = do
              res <- step x a
              case res of
                Success x -> do
                  b <- done x
                  let p@(Parse s i d) = f b
                  case i of
                    Success x -> return $ Success $ Right (x, p)
                    Partial x -> return $ Partial $ Right (x, p)
                Partial x -> return $ Partial $ Left x
            step' (Right (x, p@(Parse s i d))) a = do
              res <- s x a
              case res of
                Success x -> return $ Success $ Right (x, p)
                Partial x -> return $ Partial $ Right (x, p)
            initial' = fmap Left <$> initial
            done' (Left _) = error "The parsing is not done yet"
            done' (Right (x, Parse _ _ d)) = d x
        in Parse step' initial' done'

-}

-- There are two Alternative instances possible:
-- 1) Get first succeeding fold
-- 2) Get all succeeding folds (to get all possible parses)
{-
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
