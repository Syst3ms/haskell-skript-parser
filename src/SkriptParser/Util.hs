{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SkriptParser.Util (
  Todo,
  todo,
  always,
  fst3, snd3, thd3,
  compareListLength,
  intersections,
  check,
  (<$$>)
) where

import Data.Map (Map)
import Data.Foldable (Foldable(foldl'))
import qualified Data.Map as M

type Todo = ()

todo :: a
todo = error "TODO"

always :: a -> Bool
always = const True

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
  
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
  
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c
  
compareListLength :: [a] -> [b] -> Ordering
compareListLength []     []     = EQ
compareListLength []     (_:_)  = LT
compareListLength (_:_)  []     = GT
compareListLength (_:r1) (_:r2) = compareListLength r1 r2
  
intersections :: (Foldable t, Ord k) => t (Map k v) -> Map k v
intersections = foldl' M.intersection M.empty

check :: (a -> Bool) -> Bool -> Bool -> [a] -> Bool
check p inv isAnd = (inv /=) . (if isAnd then and else or) . map p

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap