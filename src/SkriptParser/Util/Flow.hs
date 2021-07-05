

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SkriptParser.Util.Flow where

import Control.Monad.State
import Control.Monad.Random.Lazy
import SkriptParser.Util.Exists (Wished)

type Flow s = FlowT s Wished

newtype FlowT s m a = Flow { unFlow :: StateT (StdGen,s) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadState s (FlowT s m) where
  state f = Flow $ state $ \(g,s) -> let ~(a, s') = f s in (a, (g, s'))
  get = Flow $ state $ \(g,s) -> (s,(g,s))
  put s = Flow $ state $ \(g,_) -> ((), (g,s))

instance Monad m => MonadRandom (FlowT s m) where
  getRandomR lohi = Flow $ state $ \(g,s) -> let ~(a,g') = randomR lohi g in (a,(g',s))
  getRandom = Flow $ state $ \(g,s) -> let ~(a,g') = random g in (a,(g',s))
  getRandomRs lohi = Flow $ state $ \(g,s) -> (randomRs lohi g,(g,s))
  getRandoms = Flow $ state $ \(g,s) -> (randoms g, (g,s))
  






