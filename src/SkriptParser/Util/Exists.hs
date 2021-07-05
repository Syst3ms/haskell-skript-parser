{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module SkriptParser.Util.Exists (
  Constraint,
  Exists(..),
  Wished(..),
  doWished, fromWished, fromPromised, isPromised, isNone, isException, toMaybe, maybeTo,
  SkriptException(..),
  TypeError(..), typeError,
  ConstraintError(..), constraintError,
  MissingEvidence(..), missingEvidence
) where

import GHC.Exts (Constraint)
import SkriptParser.Util.Constraint.Classes (AllCons(..), Dict(..))
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep, eqTypeRep, typeRep, (:~~:)(..), typeOf)
import Data.Proxy (Proxy(..))
import Data.Maybe (isJust)

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

castTo :: forall c a. (c a, Typeable a) => Exists c -> Wished a
castTo (Exists (v :: b)) = case typeRep @b `eqTypeRep` typeRep @a of
  Just HRefl -> Promised v
  Nothing -> Exception $ typeError @a @b
  

data ExistsList c where
  ExistsList :: (AllCons a, c a) => [a] -> ExistsList c

harmonize :: forall c. Typeable c => [Exists c] -> Wished (ExistsList c)
harmonize [] = Exception $ missingEvidence @c
harmonize [Exists x] = Promised (ExistsList [x])
harmonize (Exists (x :: a):es) = do
  ExistsList (xs :: [b]) <- harmonize es
  case typeRep @a `eqTypeRep` typeRep @b of
    Just HRefl -> return $ ExistsList (x:xs :: [b])
    Nothing -> Exception $ typeError @a @b

(//) :: (forall a. c a => a -> r) -> Exists c -> r
f // Exists v = f v

pliftE2 :: Typeable c => (forall a. c a => a -> a -> r) -> Exists c -> Exists c -> Wished r
pliftE2 f e1 e2 = harmonize [e1, e2] >>= \(ExistsList [v1, v2]) -> return (f v1 v2)

(\\) :: (forall a. c a => a -> a) -> Exists c -> Exists c
f \\ Exists v = Exists (f v)

liftE2 :: Typeable c => (forall a. c a => a -> a -> a) -> Exists c -> Exists c -> Wished (Exists c)
liftE2 f e1 e2 = harmonize [e1, e2] >>= \(ExistsList [v1, v2]) -> return (Exists (f v1 v2))

class Typeable e => SkriptException e where
  display :: e -> String

data Wished a where
  Exception :: SkriptException e => e -> Wished a
  None :: Wished a
  Promised :: a -> Wished a

instance Show a => Show (Wished a) where
  showsPrec _ None = showString "None"
  showsPrec p (Exception e) = showParen (p>10) $ showString "Exception (" . showString (display e) . showChar ')'
  showsPrec p (Promised a) = showParen (p>10) $ showString "Promised " . showsPrec 11 a

instance Eq a => Eq (Wished a) where
  None == None = True
  Promised x == Promised y = x == y
  Exception e1 == Exception e2 = isJust $ typeOf e1 `eqTypeRep` typeOf e2
  _ == _ = False

instance Functor Wished where
  fmap f (Promised a) = Promised (f a)
  fmap _ (Exception e) = Exception e
  fmap _ None = None

instance Applicative Wished where
  pure = Promised

  Promised f  <*> Promised a  = Promised (f a)
  Promised _  <*> Exception e = Exception e
  Exception e <*> _           = Exception e
  _           <*> _           = None

instance Monad Wished where
  Promised a  >>= k = k a
  Exception e >>= _ = Exception e
  None        >>= _ = None

doWished :: b -> Wished a -> (a -> b) -> b -- Argument order changed to accomodate for large lambdas as the function argument
doWished _ (Promised x) f = f x
doWished d _            _ = d

fromWished :: a -> Wished a -> a 
fromWished _ (Promised x) = x
fromWished d _            = d
  
fromPromised :: Wished a -> a
fromPromised (Promised x) = x
fromPromised _            = error "Exists.fromPromised : not Promised" -- yuck, again

isPromised :: Wished a -> Bool
isPromised (Promised _) = True
isPromised _            = False

isNone :: Wished a -> Bool
isNone None = True
isNone _    = False

isException :: Wished a -> Bool
isException (Exception _) = True
isException _             = False

toMaybe :: Wished a -> Maybe a
toMaybe (Promised x) = Just x
toMaybe _            = Nothing

maybeTo :: Maybe a -> Wished a
maybeTo (Just x) = Promised x
maybeTo Nothing = None

data TypeError = TypeError { expected :: SomeTypeRep, actual :: SomeTypeRep } deriving (Eq, Show)
instance SkriptException TypeError where
  display (TypeError e a) = "Type error : expected type '" ++ show e ++ "' but got '" ++ show a ++ "'"
typeError :: forall e a. (Typeable e, Typeable a) => TypeError
typeError = TypeError (someTypeRep (Proxy @e)) (someTypeRep (Proxy @a))  

data ConstraintError = ConstraintError { expectedCon :: SomeTypeRep, actualTy :: SomeTypeRep } deriving (Eq, Show)
instance SkriptException ConstraintError where
  display (ConstraintError e a) = "Constraint error : type '" ++ show a ++ "' does not satisfy constraint '" ++ show e ++ "'"
constraintError :: forall c a. (Typeable c, Typeable a) => ConstraintError
constraintError = ConstraintError (someTypeRep (Proxy @c)) (someTypeRep (Proxy @a))

newtype MissingEvidence = MissingEvidence { evidenceFor :: SomeTypeRep } deriving (Eq, Show)
instance SkriptException MissingEvidence where
  display (MissingEvidence e) = "Missing evidence : couldn't find an instance of '" ++ show e ++ "' to use"
missingEvidence :: forall c. Typeable c => MissingEvidence
missingEvidence = MissingEvidence (someTypeRep (Proxy @c))
  

