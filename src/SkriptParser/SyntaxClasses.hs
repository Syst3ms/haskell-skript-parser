{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SkriptParser.SyntaxClasses where

import SkriptParser.Util.Exists (Exists(..), Wished(..), typeError)
import SkriptParser.Matching.PatternMatching (MatchContext)
import Data.Text.Lazy (Text)
import SkriptParser.Util.Dynamic (Dynamic)
import Type.Reflection (Typeable, SomeTypeRep, someTypeRep)
import SkriptParser.Util.Flow (Flow)
import Control.Monad.Trans (lift)
import Data.Proxy (Proxy(..))
import SkriptParser.Util.Constraint.Classes (AllCons)
import Control.Monad.Random (uniform)

class TriggerContext c where
  name :: c -> String

class AllCons e => SyntaxElement e where
  init :: [Exists Expression] -> Int -> MatchContext -> Wished e
  toText :: e -> Maybe (Exists TriggerContext) -> Bool -> Text

data ChangeValues = Set [Dynamic] | Add [Dynamic] | Remove [Dynamic] | RemoveAll [Dynamic] | Delete | Reset
  deriving (Eq, Show)

class SyntaxElement e => Expression e where
  {-# MINIMAL change, (get | getSingle) #-}
  type Ret e
  change :: ChangeValues -> Flow (Exists TriggerContext) e

  get :: Typeable (Ret e) => e -> Flow (Exists TriggerContext) [Ret e]
  get expr = return <$> getSingle expr

  getSingle :: Typeable (Ret e) => e -> Flow (Exists TriggerContext) (Ret e)
  getSingle expr = get expr >>= \case
    [x] -> lift $ Promised x
    [] -> lift None
    xs -> if isAndList expr
     then uniform xs
     else lift $ Exception $ typeError @(Ret e) @[Ret e]

  getAllPossible :: Typeable (Ret e) => e -> Flow (Exists TriggerContext) [Ret e]
  getAllPossible = get

  isAndList :: Typeable (Ret e) => e -> Bool
  isAndList _ = True

  convert :: forall e' a. (Expression e', Ret e' ~ a) => e -> Maybe e'
  convert _ = undefined

returnType :: forall e. Typeable (Ret e) => e -> SomeTypeRep
returnType _ = someTypeRep (Proxy @(Ret e))

