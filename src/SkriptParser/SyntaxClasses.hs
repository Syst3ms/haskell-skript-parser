{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module SkriptParser.SyntaxClasses where

import SkriptParser.Util.Exists (Exists(..))
import Data.Text.Lazy (Text)
import SkriptParser.Util.Dynamic (Dynamic)
import SkriptParser.Util.Monad.Flow (Flow, (>>~))
import Control.Monad.Random (uniform)
import Data.Void (Void)
import Control.Monad.State (evalStateT)
import SkriptParser.Util.Monad.WishedT (WishedT(..), liftW)
import SkriptParser.Util.Wished (Wished(..), maybeTo)

type ToText = Maybe (Exists TriggerContext) -> Bool -> Text
type Flux = Flow (Exists TriggerContext)

class TriggerContext c where
  name :: c -> String

data ChangeValues = Set [Dynamic] | Add [Dynamic] | Remove [Dynamic] | RemoveAll [Dynamic] | Delete | Reset
  deriving (Eq, Show)

data Expression = Expression {
  exprId :: Text,
  get :: Flux [Dynamic],
  getAllPossible :: Flux [Dynamic],
  getSingle :: Flux Dynamic,
  toText :: ToText,
  isAndList :: Bool,
  change :: ChangeValues -> Flux Expression
}

defaultExpr :: Expression
defaultExpr = Expression {
    exprId = error "No identifier given!",
    get = error "Method get() not set !",
    getAllPossible = error "Method getAllPossible() not set !",
    getSingle = error "Method getSingle() not set !",
    toText = error "Method toText() not set !",
    isAndList = True,
    change = \_ -> return defaultExpr
  }

withGet :: Expression  -- original
        -> ToText
        -> Flux [Dynamic] -- get
        -> Expression  -- modified
withGet expr toText get = expr {
  get = get,
  getAllPossible = get,
  toText = toText,
  getSingle = getToGetSingle (isAndList expr) get
}

withGetSingle :: Expression  -- original
              -> ToText -- toText
              -> Flux Dynamic -- getSingle
              -> Expression  -- modified
withGetSingle expr toText getSingle = expr {
  getSingle = getSingle,
  get = return <$> getSingle,
  getAllPossible = return <$> getSingle,
  toText = toText
}

fromGet :: ToText -> Flux [Dynamic] -> Expression
fromGet = withGet defaultExpr

fromGetSingle :: ToText -> Flux Dynamic -> Expression
fromGetSingle = withGetSingle defaultExpr

getToGetSingle :: Bool -> Flux [Dynamic] -> Flux Dynamic
getToGetSingle isAnd get = get >>= \case
    [x] -> liftW $ Promised x :: Flux Dynamic
    [] -> liftW None :: Flux Dynamic
    xs -> if isAnd
      then fail "Amount error : tried retrieving a single value, but multiple were found"
      else uniform xs

data Statement = Statement {
  statementId :: Text,
  next :: Wished Statement,
  parent :: Wished Section,
  walk :: Flux Statement,
  run :: Flux ()
}
data Section = Section Statement [Statement]

defaultStatement :: Statement
defaultStatement = Statement {
  statementId = error "No identifier given!",
  next = None,
  parent = None,
  walk = error "Method walk() not implemented!",
  run = error "Method run() not implemented!" 
}

defaultSection :: Section 
defaultSection = Section defaultStatement []

withRun :: Statement -> Maybe Statement -> Text -> Flux () -> Statement
withRun st nxt sid run = st { statementId = sid, walk = runToWalk nxt run }

fromRun :: Maybe Statement -> Text -> Flux () -> Statement
fromRun = withRun defaultStatement

runToWalk :: Maybe Statement -> Flux () -> Flux Statement
runToWalk nxt run = run >>~ \case
  Promised ((), ctx) -> maybeTo $ (,ctx) <$> nxt
  None -> None
  Exception e -> Exception e

runAll :: Statement -> Exists TriggerContext -> IO (Wished Void)
runAll st ctx = go (\s -> runWishedT $ evalStateT (walk s) ctx) st
  where go f s = f s >>= \case
          Promised x -> go f x
          None -> return None
          Exception e -> return $ Exception e