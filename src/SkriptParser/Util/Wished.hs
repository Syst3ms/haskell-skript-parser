{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving, DeriveTraversable #-}

module SkriptParser.Util.Wished where

import Type.Reflection (Typeable, SomeTypeRep, typeOf, eqTypeRep, someTypeRep)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Control.Applicative (Alternative)

class Typeable e => SkriptException e where
  display :: e -> String

data Wished a where
  Exception :: SkriptException e => e -> Wished a
  None :: Wished a
  Promised :: a -> Wished a

deriving instance Foldable Wished
deriving instance Functor Wished
deriving instance Traversable Wished

instance Eq1 Wished where
  liftEq eq (Promised a) (Promised b) = eq a b
  liftEq _ None None = True
  liftEq _ (Exception _) (Exception _) = True
  liftEq _ _ _ = False
  
instance Ord1 Wished where
  liftCompare _   (Exception _) (Exception _) = EQ
  liftCompare _   (Exception _) _             = LT
  liftCompare _   _             (Exception _) = GT
  liftCompare _   None          None          = EQ
  liftCompare _   None          _             = LT
  liftCompare _   _             None          = GT  
  liftCompare cmp (Promised a)  (Promised b)  = cmp a b

instance Show1 Wished where
  liftShowsPrec _  _ _ None          = showString "None"
  liftShowsPrec _  _ p (Exception e) = showParen (p>10) $ 
                                        showString "Exception (" . showString (display e) . showChar ')'
  liftShowsPrec sp _ p (Promised x)  = showsUnaryWith sp "Promised" p x
    
instance Ord a => Ord (Wished a) where
  Exception _ <= _           = True
  _           <= Exception _ = False
  _           <= None        = False
  None        <= _           = True
  Promised a  <= Promised b  = a <= b

instance Show a => Show (Wished a) where
  showsPrec _ None = showString "None"
  showsPrec p (Exception e) = showParen (p>10) $ showString "Exception (" . showString (display e) . showChar ')'
  showsPrec p (Promised a) = showParen (p>10) $ showString "Promised " . showsPrec 11 a

instance Eq a => Eq (Wished a) where
  None == None = True
  Promised x == Promised y = x == y
  Exception e1 == Exception e2 = isJust $ typeOf e1 `eqTypeRep` typeOf e2
  _ == _ = False

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

instance MonadFail Wished where
  fail s = Exception $ GenericError s
  
instance Alternative Wished where

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

the :: [a] -> Wished a
the [x] = Promised x
the _ = None

the2 :: [a] -> Wished (a,a)
the2 [x,y] = Promised (x,y)
the2 _ = None

data TypeError = TypeError { expected :: SomeTypeRep, actual :: SomeTypeRep } deriving (Eq, Show)
instance SkriptException TypeError where
  display (TypeError e a) = "Type error: expected type '" ++ show e ++ "' but got '" ++ show a ++ "'"
typeError :: forall e a. (Typeable e, Typeable a) => TypeError
typeError = TypeError (someTypeRep (Proxy @e)) (someTypeRep (Proxy @a))  

data ConstraintError = ConstraintError { expectedCon :: SomeTypeRep, actualTy :: SomeTypeRep } deriving (Eq, Show)
instance SkriptException ConstraintError where
  display (ConstraintError e a) = "Constraint error: type '" ++ show a ++ "' does not satisfy constraint '" ++ show e ++ "'"
constraintError :: forall c a. (Typeable c, Typeable a) => ConstraintError
constraintError = ConstraintError (someTypeRep (Proxy @c)) (someTypeRep (Proxy @a))

newtype MissingEvidence = MissingEvidence { evidenceFor :: SomeTypeRep } deriving (Eq, Show)
instance SkriptException MissingEvidence where
  display (MissingEvidence e) = "Missing evidence: couldn't find an instance of '" ++ show e ++ "' to use"
missingEvidence :: forall c. Typeable c => MissingEvidence
missingEvidence = MissingEvidence (someTypeRep (Proxy @c))

newtype InvalidOperation = InvalidOperation { message :: String }
instance SkriptException InvalidOperation where
  display (InvalidOperation msg) = "Invalid operation: " ++ msg

newtype GenericError = GenericError { errMessage :: String }
instance SkriptException GenericError where
  display (GenericError msg) = "Error: " ++ msg