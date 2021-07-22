{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module SkriptParser.Util.Exists (
  Constraint,
  Exists(..),
  (//), pliftEx2, (\\), liftEx2
) where

import GHC.Exts (Constraint)
import SkriptParser.Util.Constraint.Classes (AllCons(..))
import Type.Reflection (Typeable, eqTypeRep, typeRep, (:~~:)(..))
import SkriptParser.Util.Wished

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

pliftEx2 :: Typeable c => (forall a. c a => a -> a -> r) -> Exists c -> Exists c -> Wished r
pliftEx2 f e1 e2 = harmonize [e1, e2] >>= \(ExistsList [v1, v2]) -> return (f v1 v2)

(\\) :: (forall a. c a => a -> a) -> Exists c -> Exists c
f \\ Exists v = Exists (f v)

liftEx2 :: Typeable c => (forall a. c a => a -> a -> a) -> Exists c -> Exists c -> Wished (Exists c)
liftEx2 f e1 e2 = harmonize [e1, e2] >>= \(ExistsList [v1, v2]) -> return (Exists (f v1 v2))


  

