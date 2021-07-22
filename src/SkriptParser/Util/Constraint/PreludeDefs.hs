  
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module SkriptParser.Util.Constraint.PreludeDefs  where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity)
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable (Bitraversable)
import Data.Data (Data)
import Data.Functor.Classes
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic, Generic1)
import SkriptParser.Util.Constraint.Classes
import SkriptParser.Types (Arithmetic)

{-
  Declarations for Data.Constraint
-}
instance Monoid a :=> Monad ((,) a) where ins = Sub Dict
instance Ord e :=> Ord1 (Either e) where ins = Sub Dict
instance Eq e :=> Eq1 (Either e) where ins = Sub Dict
instance Read e :=> Read1 (Either e) where ins = Sub Dict
instance Show e :=> Show1 (Either e) where ins = Sub Dict
instance (Monoid a, Monoid b) :=> Monad ((,,) a b) where ins = Sub Dict
instance (Monoid a, Monoid b) :=> Applicative ((,,) a b) where ins = Sub Dict
instance Monoid b :=> Monoid (a -> b) where ins = Sub Dict
instance Semigroup b :=> Semigroup (a -> b) where ins = Sub Dict


{-
  Declarations for AllCons
-}
instance AllCons () where reify = proofMap @() @'[Eq, Ord, Monoid, Semigroup, Enum, Show, Read, Bounded, Data]
instance AllCons Void where reify = proofMap @Void @'[Eq, Ord, Semigroup, Read, Show, Generic, Data]
instance AllCons Bool where reify = proofMap @Bool @'[Bounded, Enum, Eq, Data, Ord, Read, Show, Generic]
instance AllCons Ordering where
  reify = proofMap @Ordering @'[Bounded, Enum, Eq, Data, Ord, Read, Show, Generic, Semigroup, Monoid, Generic, Data]
instance AllCons Char where reify = proofMap @Char @'[Bounded, Enum, Eq, Data, Ord, Read, Show]
instance AllCons Int where reify = proofMap @Int @'[Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Arithmetic]
instance AllCons Integer where reify = proofMap @Integer @'[Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Arithmetic]
instance AllCons Float where
  reify = proofMap @Float @'[Enum, Eq, Floating, Fractional, Data, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Arithmetic]
instance AllCons Double where reify = reify @Float
instance AllCons Word where reify = proofMap @Word @'[Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Arithmetic]
---
instance AllCons a => AllCons [a] where
  reify = proofMap @[a] @'[Monoid, Semigroup]
            <> passthrough @'[Eq, Ord, Show, Read] @a
instance AllCons a => AllCons (Maybe a) where
  reify = proofMap @(Maybe a) @'[Generic] 
            <> passthrough @'[Eq, Data, Ord, Read, Show, Semigroup, Monoid] @a
instance AllCons a => AllCons (Identity a) where
  reify = proofMap @(Identity a) @'[Generic] 
            <> passthrough @'[Bounded, Enum, Eq, Floating, Fractional, Integral, IsString, Data, Num, Ord, Read, Real, 
                              RealFloat, RealFrac, Show, Semigroup, Monoid] @a
instance (AllCons a, AllCons b) => AllCons (Either a b) where
  reify = proofMap @(Either a b) @'[Generic, Semigroup] 
            <> passthrough2 @'[Eq, Data, Ord, Read, Show] @a @b
instance (AllCons a, AllCons b) => AllCons (a,b) where
  reify = passthrough2 @'[Eq, Monoid, Ord, Semigroup, Show, Read, Bounded] @a @b
instance (AllCons a, AllCons b) => AllCons (a -> b) where
  reify = findDeps @(a -> b) @b @'[Monoid ':> Monoid, Semigroup ':> Semigroup]
---
instance (AllCons a, AllCons b, AllCons c) => AllCons (a,b,c) where
  reify = passthroughs @'[Eq, Monoid, Ord, Semigroup, Show, Read, Bounded, Data] @'[a, b, c]
---------
instance AllCons Maybe where
  reify = proofMap @Maybe @'[Monad, Functor, MonadFail, MonadFix, Applicative, Foldable, Traversable, MonadPlus, 
                             Alternative, Show1, Read1, Ord1, Eq1, Generic1]
instance AllCons [] where reify = proofMap @[] @'[Applicative, Functor, Monad, Foldable, MonadFail, Traversable, Generic1]
instance AllCons Identity where
  reify = proofMap @Identity @'[Monad, Functor, MonadFix, Applicative, Foldable, Traversable, Show1, Read1, Ord1, Eq1, 
                                Generic1]
---
instance AllCons a => AllCons ((,) a) where
  reify = proofMap @((,) a) @'[Functor, Foldable, Traversable, Generic1] 
            <> findDeps @((,) a) @a @'[Monoid ':> Applicative, Monoid ':> Monad]
instance AllCons e => AllCons (Either e) where
  reify = proofMap @(Either e) @'[Monad, Functor, MonadFix, Applicative, Foldable, Traversable, Generic1] 
            <> findDeps @(Either e) @e @'[Show ':> Show1, Read ':> Read1, Eq ':> Eq1, Ord ':> Ord1]
instance AllCons a => AllCons ((->) a) where reify = proofMap @((->) a) @'[Functor, Applicative, Monad, MonadFix]         
---
instance (AllCons a, AllCons b) => AllCons ((,,) a b) where
  reify = proofMap @((,,) a b) @'[Functor, Generic1] 
            <> findMDeps @((,,) a b) @'[a, b] @'[[Monoid, Monoid] ':> Applicative, [Monoid, Monoid] ':> Monad]
---------
instance AllCons Either where reify = proofMap @Either @'[Show2, Read2, Ord2, Eq2, Bifunctor, Bifoldable, Bitraversable]
---
instance AllCons a => AllCons ((,,) a) where reify = proofMap @((,,) a) @'[Bifunctor, Bifoldable, Bitraversable]
  
