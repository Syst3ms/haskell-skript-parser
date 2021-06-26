{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}

module SkriptParser.Util.Dynamic where

import Prelude
import Type.Reflection
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Constraint ((:-)(Sub), Dict(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (isJust, isJust)
import Data.Functor (($>))
import Data.Typeable (cast)

{-
data Dynamic where
  Dynamic :: (Typeable a, Typeable cs) => TypeRep a -> Evidence cs a -> a -> Dynamic

instance Show Dynamic where
  showsPrec p (Dynamic tr _ _) = showParen (p > 11) $ showString "<<" . shows tr . showString ">>"

-- | Creates a Dynamic instance from a type compatible with @AllCons@. Saves every constraint this type is subject to
--   as Evidence, that can be reused later on if needed. 
toDyn :: forall a. (Typeable (AllCons a), (AllCons a) a, Typeable a) => 
         a -> Dynamic
toDyn = Dynamic (typeRep @a) (createEvidence @a)

toDynExtraConstraint :: forall cs a. (Typeable (AllCons a), Typeable cs, Typeable a, (AllCons a) a, cs a) =>
                        a -> Dynamic
toDynExtraConstraint = Dynamic (typeRep @a) (Evidence @(cs :&: AllCons a))                        

-- | Creates a Dynamic instance where only the specified constraint is saved. 
toDynConstraint :: forall cs a. (Typeable cs, cs a, Typeable a) => a -> Dynamic
toDynConstraint = Dynamic (typeRep @a) (Evidence @cs @a)

-- All of the following code is absolutely horrendous, unsafe shit everywhere. The cause of this is that the type system
-- doesn't understand what HasCons means, so we have to manually assure Haskell that all of this is alright.

combinedRep :: forall k. Typeable k => TypeRep ((:&:) :: CG k -> CG k -> CG k)
combinedRep = typeRep @(:&:)

consComb :: forall k (a :: k). Typeable k => SomeTypeRep -> Maybe (SomeTypeRep, SomeTypeRep)
consComb (SomeTypeRep (App (App con a) b)) = (SomeTypeRep a, SomeTypeRep b) <$ con `eqTypeRep` combinedRep @k
consComb _ = Nothing

hasCons :: forall k (a :: k). Typeable k => SomeTypeRep -> SomeTypeRep -> Bool
hasCons c1@(SomeTypeRep ca) c2@(SomeTypeRep cb) = case (consComb @k c2, consComb @k c1) of 
  (Just (a',b'), _) -> hasCons @k c1 a' && hasCons @k c1 b'
  (_, Just (a,b))   -> hasCons @k a c2 || hasCons @k b c2
  _                 -> isJust $ ca `eqTypeRep` cb

-- | Checks if a given Evidence is evidence for a given constraint
asEvidenceOf :: forall c2 c1 (a :: Type).
                (Typeable a, Typeable c1, Typeable c2) => Evidence c1 a -> Maybe (Evidence c2 a)
asEvidenceOf ev = case typeOf ev of
  App (App _ c1) _ -> if hasCons @Type (SomeTypeRep c1) (someTypeRep (Proxy @c2)) then Just $ unsafeCoerce ev else Nothing

-- | If it is known that constraint c1 implies constraint c, and that constraint c entails constraint c2,
--   then we can safely convert evidence for c1 into evidence for c and c2
upcast :: forall c2 c1 c (a :: Type). 
          (c1 `HasCons` c ~ 'True, Typeable c2) => 
          c a :- c2 a -> Evidence c1 a -> Evidence (c :&: c2) a
upcast (Sub d) e = case refinement e :: Evidence c a of Evidence -> case d of Dict -> Evidence -- Order matters here !

-- | Unwraps a Dynamic value into the requested type, or type-errors
--   Specify the desired type using TypeApplications or a type signature
wished :: forall a. Typeable a => Dynamic -> Wished a
wished (Dynamic tr _ val) = case (tr `eqTypeRep` typeRep @a, tr) of
  (Just HRefl, _) -> Promised val
  (_, r@(App con wish)) -> TypeError aTypeRep  $ case con `eqTypeRep` typeRep @Wished of
     -- Oh boy... If the dynamic value is constructed, we see if by any chance it's a wish for another type
     -- If so, we can refine the error and say that the 'actual' component of the error was that other type
     -- Otherwise, just proceed without refinement
         Just HRefl -> SomeTypeRep wish
         Nothing -> SomeTypeRep r
  (_, r) -> TypeError aTypeRep (SomeTypeRep r)
  where aTypeRep = someTypeRep (Proxy @a)

wishedCon :: forall c. Typeable c => Dynamic -> Wished (TExists c)
wishedCon (Dynamic tr ev val) = case asEvidenceOf @c ev of
  Just Evidence -> Promised (TExists val)
  Nothing -> ConError (someTypeRep (Proxy @c)) (SomeTypeRep tr)
{-
  (Just e, _) -> Promised e
  (_, r@(SomeTypeRep (App con wish))) -> case (con `eqTypeRep` typeRep @Wished, wish) of
      (Just HRefl, _) -> TypeError exTypeRep (SomeTypeRep wish)
      -- Here we can further refine the error if the value turns out to be a wished constrained value, but for some other
      -- constraint. In that case, we can be very precise and report a constraint error.
      (Nothing, App con' maybeCs) -> case con' `eqTypeRep` typeRep @TExists of
        Just HRefl -> ConError (someTypeRep (Proxy @c)) (SomeTypeRep maybeCs)
        Nothing -> TypeError exTypeRep r
      _ -> TypeError exTypeRep r
  (_, r)  -> TypeError exTypeRep r
  where exTypeRep = someTypeRep (Proxy @(TExists c))
-}

infixl 4 ~>
-- | Lifts a typed unary function to the Dynamic level
(~>) :: Typeable a => (a -> b) -> Dynamic -> Wished b
f ~> da = f <$> wished da

infixl 4 <~>
(<~>) :: Typeable a => Wished (a -> b) -> Dynamic -> Wished b
wf <~> da = wf <*> wished da

-- | Lifts a typed binary function to the Dynamic level
liftD2 :: (Typeable a, Typeable b) => (a -> b -> c) -> Dynamic -> Dynamic -> Wished c
liftD2 f da db = f ~> da <~> db
                
-- | Lifts a typed trinary function to the Dynamic level
liftD3 :: (Typeable a, Typeable b, Typeable c) =>
          (a -> b -> c -> d) ->
          Dynamic -> Dynamic -> Dynamic -> Wished d
liftD3 f da db dc = f ~> da <~> db <~> dc

newtype IsSingle = IsSingle Bool  

data Wished' a = TypeError SomeTypeRep SomeTypeRep
               | WrongNumber IsSingle
               | Promised a
-}               