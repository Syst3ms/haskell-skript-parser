{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SkriptParser.Util.Dynamic where

import Prelude
import Type.Reflection
import Data.Proxy (Proxy(..))
import Data.Constraint (Dict(..))
import SkriptParser.Util.Constraint.Classes (AllCons(..), getProofTR, proofMap)
import SkriptParser.Util.Constraint.PreludeDefs ()
import SkriptParser.Util.Exists (Exists(..))
import SkriptParser.Util.Wished (Wished(..), TypeError(..), ConstraintError(..))

data Dynamic where
  Dynamic :: AllCons a => TypeRep a -> a -> Dynamic

instance Show Dynamic where
  showsPrec p (Dynamic tr _) = showParen (p > 11) $ showString "<<" . shows tr . showString ">>"
instance Eq Dynamic where
  Dynamic trX x == Dynamic trY y = case trX `eqTypeRep` trY of
    Nothing -> False
    Just HRefl -> case getProofTR @Eq trX of
      Nothing -> False 
      Just Dict -> x == y
instance AllCons Dynamic where
  reify = proofMap @Dynamic @'[Eq, Show]
      
      
-- | Creates a Dynamic instance from a type compatible with @AllCons@. Saves every constraint this type is subject to
--   as Evidence, that can be reused later on if needed.
toDyn :: forall a. AllCons a =>
         a -> Dynamic
toDyn = Dynamic (typeRep @a)

exToDyn :: Exists c -> Dynamic
exToDyn (Exists v) = toDyn v

-- |
--  Unwraps a Dynamic value into the requested type, or type-errors
--   Specify the desired type using TypeApplications or a type signature
wished :: forall a. Typeable a => Dynamic -> Wished a
wished (Dynamic tr val) = case tr `eqTypeRep` typeRep @a of
  Just HRefl -> Promised val
  Nothing -> Exception $ TypeError (someTypeRep (Proxy @a)) (SomeTypeRep tr)

wishedCon :: forall c. Typeable c => Dynamic -> Wished (Exists c)
wishedCon (Dynamic tr val) = case getProofTR @c tr of
  Just Dict -> Promised (Exists val)
  Nothing -> Exception $ ConstraintError (someTypeRep (Proxy @c)) (SomeTypeRep tr)

infixl 4 ~>
-- | Lifts a typed unary function to the Dynamic level
(~>) :: (Typeable a, AllCons b) => (a -> b) -> Dynamic -> Wished Dynamic
f ~> da = toDyn <$> (f <$> wished da)

-- | Lifts a typed binary function to the Dynamic level
liftD2 :: (Typeable a, Typeable b, AllCons c) => (a -> b -> c) -> Dynamic -> Dynamic -> Wished Dynamic
liftD2 f da db = toDyn <$> (f <$> wished da <*> wished db)

-- | Lifts a typed trinary function to the Dynamic level
liftD3 :: (Typeable a, Typeable b, Typeable c, AllCons d) =>
          (a -> b -> c -> d) ->
          Dynamic -> Dynamic -> Dynamic -> Wished Dynamic
liftD3 f da db dc = toDyn <$> (f <$> wished da <*> wished db <*> wished dc)