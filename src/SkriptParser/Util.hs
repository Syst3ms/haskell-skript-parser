{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SkriptParser.Util (
  Todo,
  todo,
  always,
  fst3, snd3, thd3,
  compareLength,
  compareListLength,
  intersections
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

compareLength :: [a] -> Int -> Ordering
compareLength [] n = compare 0 n
compareLength (_:r) n
  | n <= 0 = GT
  | otherwise = compareLength r (pred n)
  
intersections :: (Foldable f, Ord k) => f (Map k v) -> Map k v
intersections = foldl' M.intersection M.empty
