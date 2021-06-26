{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module SkriptParser.Util.Exists (
  Constraint,
  Exists(..),
  Wished(..),
  ErrComponent(..),
  requestConstraint
) where

import GHC.Exts (Constraint)
import SkriptParser.Util.Constraint.Classes (AllCons(..), Dict(..), getProof)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep, eqTypeRep, typeRep, (:~~:)(..))
import Data.Proxy (Proxy(..))
import Data.Kind (Type)

data ErrComponent = Typ SomeTypeRep | Constr SomeTypeRep deriving (Eq, Ord, Show)
data Wished a = TypeError { expected :: SomeTypeRep, actual :: SomeTypeRep }
              | ConstraintError SomeTypeRep -- Desired
              | MissingEvidence
              | Promised a            
  deriving (Eq, Ord, Show)

instance Functor Wished where
  fmap f (Promised a) = Promised (f a)
  fmap _ (TypeError e a) = TypeError e a
  fmap _ (ConstraintError e) = ConstraintError e
  fmap _ MissingEvidence = MissingEvidence

instance Applicative Wished where
  pure a = Promised a

  Promised f <*> Promised a = Promised (f a)
  Promised _ <*> TypeError e a = TypeError e a
  Promised _ <*> ConstraintError e = ConstraintError e 
  TypeError e a <*> _ = TypeError e a
  ConstraintError e <*> _ = ConstraintError e
  _ <*> _ = MissingEvidence

instance Monad Wished where
  Promised a >>= k = k a
  TypeError e a >>= _ = TypeError e a
  ConstraintError e >>= _ = ConstraintError e
  _ >>= _ = MissingEvidence

-- | A datatype which holds a value of a type satisfying the constraint 'c' and AllCons, in such a way that evidence
--   for these constraints can be retrieved by pattern matching.
-- 
--   Example:
--
--   > foo :: Exists Show
--   > foo = Exists (Just 9 :: Maybe Int)
--   >
--   > printExists :: Exists Show -> IO ()
--   > printExists (Exists e) = print e
--   >
--   > main = printExists foo -- prints "Just 9"
data Exists c where
     Exists :: (AllCons a, c a) => a -> Exists c

instance Show (Exists Show) where
  showsPrec p (Exists s) = showParen (p>10) $ showString "Exists " . showsPrec 11 s

requestConstraint :: forall c a. (Typeable c, AllCons a) => a -> Maybe (Exists c)
requestConstraint a = case getProof @c @a of
  Just Dict -> Just (Exists a)
  Nothing -> Nothing

convert :: forall c1 c2. (Typeable c1, Typeable c2) => Exists c1 -> Wished (Exists c2)
convert (Exists (e :: a)) = case getProof @c2 @a of
  Just Dict -> Promised (Exists e)
  Nothing -> ConstraintError (someTypeRep (Proxy @c1))

data ExistsList c where
  ExistsList :: (AllCons a, c a) => [a] -> ExistsList c

harmonize :: forall c. [Exists c] -> Wished (ExistsList c)
harmonize [] = MissingEvidence
harmonize [Exists x] = Promised (ExistsList [x])
harmonize (Exists (x :: a):es) = do
  ExistsList (xs :: [b]) <- harmonize es
  case typeRep @a `eqTypeRep` typeRep @b of
    Just HRefl -> return $ ExistsList (x:xs :: [b])
    Nothing -> TypeError (someTypeRep (Proxy @a)) (someTypeRep (Promised @b))

(//) :: (forall a. c a => a -> r) -> Exists c -> r
f // Exists v = f v

pliftE2 :: (forall a. c a => a -> a -> r) -> Exists c -> Exists c -> Wished r
pliftE2 f e1 e2 = harmonize [e1, e2] >>= \(ExistsList [v1, v2]) -> return (f v1 v2)

(\\) :: (forall a. c a => a -> a) -> Exists c -> Exists c
f \\ Exists v = Exists (f v)

liftE2 :: (forall a. c a => a -> a -> a) -> Exists c -> Exists c -> Wished (Exists c)
liftE2 f e1 e2 = harmonize [e1, e2] >>= \(ExistsList [v1, v2]) -> return (Exists (f v1 v2))


