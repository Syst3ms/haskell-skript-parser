{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SkriptParser.Util.Monad.WishedT where

import SkriptParser.Util.Wished (Wished(..), GenericError(..), fromWished)
import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Control.Monad.Random.Class (MonadRandom(..))
import Control.Monad.State (StateT(..))
import Data.Functor.Classes

newtype WishedT m a = WishedT { runWishedT :: m (Wished a) }

-- | Transform the computation inside a @WishedT@.
--
-- * @'runWishedT' ('mapWishedT' f m) = f ('runWishedT' m)@
mapWishedT :: (m (Wished a) -> n (Wished b)) -> WishedT m a -> WishedT n b
mapWishedT f = WishedT . f . runWishedT
{-# INLINE mapWishedT #-}

instance Eq1 m => Eq1 (WishedT m) where
  liftEq eq (WishedT a) (WishedT b) = liftEq (liftEq eq) a b
instance Ord1 m => Ord1 (WishedT m) where
  liftCompare cmp (WishedT a) (WishedT b) = liftCompare (liftCompare cmp) a b
instance Show1 m => Show1 (WishedT m) where
  liftShowsPrec sp sl p (WishedT a) = showsUnaryWith (liftShowsPrec sp' sl') "WishedT" p a
    where sp' = liftShowsPrec sp sl
          sl' = liftShowList sp sl
instance (Eq1 m, Eq a) => Eq (WishedT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (WishedT m a) where compare = compare1
instance (Show1 m, Show a) => Show (WishedT m a) where showsPrec = showsPrec1

instance Functor m => Functor (WishedT m) where
    {-# SPECIALIZE instance Functor (WishedT IO) #-}
    fmap f = mapWishedT (fmap (fmap f))
    {-# INLINE fmap #-}

instance Foldable f => Foldable (WishedT f) where
    foldMap f (WishedT a) = foldMap (foldMap f) a
    {-# INLINE foldMap #-}

instance Traversable f => Traversable (WishedT f) where
    traverse f (WishedT a) = WishedT <$> traverse (traverse f) a
    {-# INLINE traverse #-}

instance Monad m => Applicative (WishedT m) where
    {-# SPECIALIZE instance Applicative (WishedT IO) #-}
    pure = WishedT . return . Promised
    {-# INLINE pure #-}
    mf <*> mx = WishedT $ do
        mb_f <- runWishedT mf
        case mb_f of
            Exception e -> return (Exception e)
            None -> return None
            Promised f  -> do
                mb_x <- runWishedT mx
                case mb_x of
                    Exception e -> return (Exception e)
                    None -> return None
                    Promised x  -> return (Promised (f x))
    {-# INLINE (<*>) #-}
    (*>) = (>>)
    {-# INLINE (*>) #-}

instance Monad m => Alternative (WishedT m) where
    {-# SPECIALIZE instance Alternative (WishedT IO) #-}
    empty = WishedT (return None)
    {-# INLINE empty #-}
    x <|> y = WishedT $ do
        v <- runWishedT x
        case v of
            Promised _ -> return v
            _ -> runWishedT y
    {-# INLINE (<|>) #-}

instance Monad m => Monad (WishedT m) where
    {-# SPECIALIZE instance Monad (WishedT IO) #-}
    x >>= f = WishedT $ do
        v <- runWishedT x
        case v of
            Exception e -> return (Exception e)
            None -> return None
            Promised y  -> runWishedT (f y)
    {-# INLINE (>>=) #-}

instance Monad m => MonadFail (WishedT m) where
    {-# SPECIALIZE instance MonadFail (WishedT IO) #-}
    fail = WishedT . return . Exception . GenericError
    {-# INLINE fail #-}

instance Monad m => MonadPlus (WishedT m) where
  {-# SPECIALIZE instance MonadPlus (WishedT IO) #-}

instance MonadFix m => MonadFix (WishedT m) where
    {-# SPECIALIZE instance MonadFix (WishedT IO) #-}
    mfix f = WishedT (mfix (runWishedT . f . fromWished bomb))
      where bomb = error "mfix (WishedT): inner computation didn't return a Promised"
    {-# INLINE mfix #-}

instance MonadTrans WishedT where
    lift = WishedT . fmap Promised
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (WishedT m) where
    {-# SPECIALIZE instance MonadIO (WishedT IO) #-}
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadZip m => MonadZip (WishedT m) where
    mzipWith f (WishedT a) (WishedT b) = WishedT $ mzipWith (liftA2 f) a b
    {-# INLINE mzipWith #-}
    
instance MonadRandom m => MonadRandom (WishedT m) where
    {-# SPECIALIZE instance MonadRandom (WishedT IO) #-}
    getRandomR = lift . getRandomR
    {-# INLINE getRandomR #-}
    getRandom = lift getRandom
    {-# INLINE getRandom #-}
    getRandomRs = lift . getRandomRs
    {-# INLINE getRandomRs #-}
    getRandoms = lift getRandoms 
    {-# INLINE getRandoms #-}
    
class Monad m => MonadWished m where
  liftW :: Wished a -> m a
  
instance MonadWished Wished where
   liftW = id
   {-# INLINE liftW #-}
instance Monad m => MonadWished (WishedT m) where
  {-# SPECIALIZE instance MonadWished (WishedT IO) #-}
  liftW = WishedT . return
  {-# INLINE liftW #-}
instance MonadWished m => MonadWished (StateT s m) where
  {-# SPECIALIZE instance MonadWished (StateT s (WishedT IO)) #-}
  liftW = lift . liftW
  {-# INLINE liftW #-}