{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module SkriptParser.Util.Constraint.Classes (
  ProofMap, SomeDict(..), (:>)(..),
  MultiC,
  TypeList(..), AllConsList(..), RespectiveCons(..),
  SDepList(..), MDepList(..),
  AllCons(..), SimpleConstraints(..),
  passthrough, passthrough2, passthroughs,
  getProof,
  
  Dict(..), (:-)(..), (:=>)(..)
) where

import Data.Map (Map, (!?))
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import Data.Constraint (Constraint, Dict(..), (:=>)(..), (:-)(..))
import Data.Typeable (Typeable, cast)
import SkriptParser.Util (intersections)
import Type.Reflection (someTypeRep, SomeTypeRep)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Kind (Type)

class Empty a
instance Empty a

type ProofMap = Map SomeTypeRep SomeDict
data SomeDict where
  SomeDict :: Typeable c => Dict c -> SomeDict
  deriving Typeable
data a :> b = a :> b

deriving instance Show SomeDict

type family MultiC (cs :: [Type -> Constraint]) (as :: [Type]) :: Constraint where
  MultiC '[] '[] = ()
  MultiC '[c] '[a] = c a
  MultiC (c ': cs) (a ': as) = (c a, MultiC cs as)

class TypeList as where
  repSet :: Set SomeTypeRep
  repList :: [SomeTypeRep]
  repList = S.toList (repSet @as)
instance TypeList '[] where repSet = S.empty
instance (Typeable x, TypeList xs) => TypeList (x ': xs) where repSet = someTypeRep (Proxy @x) `S.insert` repSet @xs

class AllConsList as where proofMapList :: [ProofMap]
instance AllConsList '[] where proofMapList = []
instance (AllCons x, AllConsList xs) => AllConsList (x ': xs) where
  proofMapList = reify @x : proofMapList @xs

class RespectiveCons cs as where
  respectiveProof :: Maybe (Dict (MultiC cs as))
instance RespectiveCons '[] '[] where respectiveProof = Just $ Dict @()
instance (Typeable c, AllCons a, RespectiveCons cs as) => RespectiveCons (c ': cs) (a ': as) where
  respectiveProof = respectiveProof @cs @as >>= \Dict ->
          case getProof @c @a of
            Just Dict -> Just Dict
            Nothing -> Nothing

class (AllCons a, AllCons t) => SDepList a t m where
  findDeps :: ProofMap
instance (AllCons a, AllCons t) => SDepList a t '[] where findDeps = M.empty
instance (AllCons a, AllCons t, Typeable c, Typeable d, SDepList a t ds, c t :=> d a) =>
         SDepList a t ((c ':> d) ': ds) where
  findDeps = case getProof @c @t of
    Just Dict -> case ins @(c t) @(d a) of
      Sub Dict -> M.insert (someTypeRep (Proxy @d)) (someDict @(d a)) rest
    Nothing -> rest
    where rest = findDeps @a @t @ds

class (AllCons a, AllConsList ts) => MDepList a ts m where
  findMDeps :: ProofMap
instance (AllCons a, AllConsList ts) => MDepList a ts '[] where findMDeps = M.empty
instance (Typeable d, MDepList a ts ds, RespectiveCons cs ts, MultiC cs ts :=> d a) => MDepList a ts ((cs ':> d) ': ds) where
  findMDeps = case respectiveProof @cs @ts of
      Just Dict -> case ins @(MultiC cs ts) @(d a) of
        Sub Dict -> M.insert (someTypeRep (Proxy @d)) (someDict @(d a)) rest
      Nothing -> rest
    where rest = findMDeps @a @ts @ds

class Typeable a => AllCons a where
  reify :: ProofMap

class SimpleConstraints a cs where proofMap :: ProofMap
instance SimpleConstraints a '[] where proofMap = M.empty
instance (Typeable c, Typeable a, c a, SimpleConstraints a cs) => SimpleConstraints a (c ': cs) where
  proofMap = M.insert (someTypeRep (Proxy @c)) (someDict @(c a)) $ proofMap @a @cs

passthrough :: forall xs a. (TypeList xs, AllCons a) => ProofMap
passthrough = reify @a `M.restrictKeys` repSet @xs

passthrough2 :: forall xs a b. (TypeList xs, AllCons a, AllCons b) => ProofMap
passthrough2 = reify @a `M.intersection` reify @b `M.restrictKeys` repSet @xs

passthroughs :: forall xs ts. (TypeList xs, AllConsList ts) => ProofMap
passthroughs = intersections (proofMapList @ts) `M.restrictKeys` repSet @xs

someDict :: forall c. (Typeable c, c) => SomeDict
someDict = SomeDict (Dict @c)

getProof :: forall c a. (Typeable c, AllCons a) => Maybe (Dict (c a))
getProof = reify @a !? someTypeRep (Proxy @c) >>= \(SomeDict d) -> cast d
