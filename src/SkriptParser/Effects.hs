{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module SkriptParser.Effects where

import SkriptParser.SyntaxClasses (Expression(..), Statement, fromRun, Flux)
import SkriptParser.Patterns.PatternMatching (MatchContext)
import SkriptParser.Util.Wished (Wished(..))
import SkriptParser.Util.Monad.WishedT (liftW)
import Type.Reflection (Typeable)
import SkriptParser.Util.Dynamic (wished, wishedCon)
import Control.Monad.State (liftIO, mapStateT)
import SkriptParser.Util.Exists (Exists)
import Data.Proxy (Proxy(..))
import System.Exit (exitSuccess)

(*^*) :: forall a. Typeable a => Expression -> Proxy a -> Flux [a]
expr *^* _ = flip mapStateT (get expr) $ \m -> do
  (ds, ctx') <- m
  liftW $ (,ctx') <$> traverse (wished @a) ds

(<^>) :: forall c. Typeable c => Expression -> Proxy c -> Flux [Exists c]
expr <^> _ = flip mapStateT (get expr) $ \m -> do
  (ds, ctx') <- m
  liftW $ (,ctx') <$> traverse (wishedCon @c) ds

buildPrint :: [Expression] -> Int -> MatchContext -> Maybe Statement -> Wished Statement
buildPrint [expr] _ _ nxt = Promised $ fromRun nxt "print" $ do
    showables <- expr <^> Proxy @Show
    liftIO (print showables)
buildPrint _ _ _ _ = None

buildShutdown :: [Expression] -> Int -> MatchContext -> Maybe Statement -> Wished Statement
buildShutdown _ _ _ nxt = Promised $ fromRun nxt "shutdown" $ liftIO exitSuccess



