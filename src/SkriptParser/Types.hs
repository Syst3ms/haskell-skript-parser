{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module SkriptParser.Types (
  Arithmetic(..),
  SkriptType(..),
  makeSimpleType,
  someTypeOf, baseName, pluralized, PatternType
) where

import SkriptParser.Util
import Type.Reflection
import SkriptParser.Util.Constraint.Classes (AllCons)

class AllCons a => Arithmetic a where
    (-|) :: a -> a -> a -- Difference
    (+.) :: a -> a -> a
    (-.) :: a -> a -> a

data SkriptType where
  SkriptType :: AllCons a => {
    typ :: TypeRep a,
    nameForms :: (String, String),
    toString :: a -> String,
    literalParser :: Maybe (String -> Maybe a)
  } -> SkriptType

instance Show SkriptType where
  showsPrec p t@SkriptType {..} = showParen (p>10) $
    showString "SkriptType { " . showString (baseName t) . showString ", " . shows typ . showString " }"
instance Eq SkriptType where
  t1 == t2 = someTypeOf t1 == someTypeOf t2

makeSimpleType :: AllCons a => TypeRep a -> (String, String) -> (a -> String) -> SkriptType
makeSimpleType tRep name str = SkriptType tRep name str Nothing

someTypeOf :: SkriptType -> SomeTypeRep
someTypeOf SkriptType {..} = SomeTypeRep typ

baseName :: SkriptType -> String
baseName = fst . nameForms

pluralized :: SkriptType -> Bool -> String
pluralized t plural = (if plural then fst else snd) (nameForms t)

newtype PatternType = PatternType (SkriptType, Bool)

deriving instance Eq PatternType

instance Show PatternType where
  show (PatternType (t,p)) = pluralized t p
  
booleanType :: SkriptType
booleanType = todo (typeOf True)














