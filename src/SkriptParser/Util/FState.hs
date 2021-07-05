{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SkriptParser.Util.FState (
  FailState,
  SelfFailState,
  runFState,
  evalFState,
  execFState,
  mapFState,
  mapSFState,
  withFState,
  packFState,
  thenFail,
  justFail,
  selfFailing,
  gets2,
  untilSuccess,
  untilSuccessWithInput,
  maybeFState,
  success
) where

import Control.Monad (join, MonadPlus(..))
import Control.Monad.State (State, MonadState(..), gets, runState, modify)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Bifunctor (first)
import Data.Foldable (asum)

-- | A State monad that keeps track of the state even in the case of failure (as that state carries information about
--   x
type FailState s a = MaybeT (State s) a
type SelfFailState s = FailState s s

runFState :: FailState s a -> s -> (Maybe a, s)
runFState = runState . runMaybeT

-- | Computes a FailState operation given an initial state and returns the computed result
evalFState :: FailState s a -> s -> Maybe a
evalFState m s = fst (runFState m s)

-- | Computes a FailState operation given an initial state and returns the final state ignoring the result
execFState :: FailState s a -> s -> s
execFState m s = snd (runFState m s)

-- | Maps a stateful operation using a function mapping a result and a state to another result (or failure) and
--   a (possibly) different state
mapFState :: ((a, s) -> (Maybe b, s)) -> FailState s a -> FailState s b
mapFState f m = do
  s <- get
  a <- m
  MaybeT (state (\_ -> f (a,s)))

-- | Maps a self-failing stateful operation to itself.
--   Since failure is borne by the state itself, the given function cannot indicate failure.
mapSFState :: ((a, s) -> s) -> FailState s a -> SelfFailState s
mapSFState f = mapFState (first Just . join (,) . f)

-- | Turns a stateful operation into another by changing its input state
withFState :: (s -> s) -> FailState s a -> FailState s a
withFState f m = MaybeT $ state $ runState (runMaybeT m) . f

-- | Turns a stateful operation into one that returns both the result and the end state
packFState :: FailState s a -> FailState s (a,s)
packFState m = do a <- m
                  s <- get
                  return (a,s)

-- | Changes the result state of a stateful operation then fails
thenFail :: (s -> s) -> FailState s a -> FailState s a
thenFail f = mapFState (\(_, s) -> (Nothing, f s)) 

justFail :: (s -> s) -> FailState s a
justFail = (>> mzero) . modify

-- | Gets multiple components of a state at once
gets2 :: (s -> a) -> (s -> b) -> FailState s (a,b)
gets2 f g = do
    s <- get
    return (f s, g s)

-- | Chains stateful operations together until one of them runs successfully (returning Just the result and the
-- corresponding input)
untilSuccessWithInput :: (Functor t, Foldable t) => (a -> FailState s b) -> t a -> FailState s (b, a)
untilSuccessWithInput makeState inputs = asum ((\x -> (,x) <$> makeState x) <$> inputs)

-- | Chains stateful operations together until one of them runs successfully (returning Just the result)
untilSuccess :: (Functor t, Foldable t) => (a -> FailState s b) -> t a -> FailState s b
untilSuccess makeState inputs = asum (makeState <$> inputs)

-- | The result of a FailState computation in which any failure is borne by an object of the state's type.
--   In case of success, the result is returned, and in case of failure, the state at that point is.  
selfFailing :: SelfFailState s -> s -> s
selfFailing m initial = case runFState m initial of
  (Just res, _) -> res
  (Nothing , s) -> s

-- | Constructs a self-failing stateful operation from a Maybe. If the Maybe value is Nothing, @mzero@ is returned.
--   If it is @Just x@, then an operation that changes the state given @x@ is returned.
maybeFState :: Maybe a -> (a -> s -> s) -> SelfFailState s
maybeFState m f = maybe mzero (gets . f) m

success :: s -> FailState s s
success s = do {put s; return s}

instance Semigroup (SelfFailState s) where
   (<>) = (>>)
   
instance Monoid (SelfFailState s) where
   mempty = get