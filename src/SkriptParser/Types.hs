{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module SkriptParser.Types
    where

import Data.Dynamic
import Data.Typeable (TypeRep)
import SkriptParser.Util
import SkriptParser.Util.Dynamic
import Type.Reflection hiding (TypeRep)
import SkriptParser.Util.Exists (Exists(..))
import SkriptParser.Util.Constraint.Classes (AllCons)

data ChangeMode = Set | Add | Remove | Delete | Reset | RemoveAll deriving (Eq, Show)

data Changer = Changer {
  acceptsChange :: Dynamic -> ChangeMode -> [TypeRep],
  change :: Dynamic -> [Dynamic]  -> ChangeMode -> Maybe [Dynamic]
}

class Typeable a => Arithmetic a where
    (-|) :: a -> a -> a -- Difference
    (+.) :: a -> a -> a
    (-.) :: a -> a -> a

data SkriptType where
  SkriptType :: {
    typ :: SomeTypeRep,
    nameForms :: (String, String),
    toString :: Dynamic -> String,
    literalParser :: Maybe (String -> Dynamic),
    changer :: Maybe Changer,
    arithmetic :: Maybe String
  } -> SkriptType

instance Arithmetic Integer where
  (-|) = (abs .) . (-)
  (+.) = (+)
  (-.) = (-)

instance Arithmetic Double where
  (-|) = (abs .) . (-)
  (+.) = (+)
  (-.) = (-)


instance Show SkriptType where
  showsPrec p t = showParen (p>10) $ 
    showString "SkriptType { " . showString (baseName t) . showString ", " . shows (typ t) . showString " }"
instance Eq SkriptType where
  (==) t1 t2 = typ t1 == typ t2  

makeSimpleType :: TypeRep -> (String, String) -> (Dynamic -> String) -> SkriptType
makeSimpleType tRep name str = SkriptType tRep name str Nothing Nothing Nothing

baseName :: SkriptType -> String
baseName = fst . nameForms

pluralized :: SkriptType -> Bool -> String
pluralized t plural = (if plural then fst else snd) (nameForms t)

newtype PatternType = PatternType (SkriptType, Bool) deriving (Eq)

instance Show PatternType where
  show (PatternType (t,p)) = pluralized t p
  
booleanType :: SkriptType
booleanType = todo (typeOf True)














