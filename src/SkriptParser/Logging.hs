{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module SkriptParser.Logging (
  ErrorType(..),
  ErrorContext(..),
  Verbosity(..),
  LogEntryType(..),
  LogEntry(..),
  Logger(isDebug, hasError, fileInfo),
  formatLog,
  makeLogEntry,
  toErrorType,
  emptyLogger,
  switchContext,
  finish,
  finalizeLogs,
  recurse,
  recurseState,
  nextLine,
  forgetError,
  clearLessThanError,
  clearErrors,
  clearNotDebug,
  logNewEntry,
  logError,
  logWarn,
  logInfo,
  logDebug
) where

import Control.Applicative (liftA2)
import Control.Monad.State (MonadState, modify, gets)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, maybeToList, isNothing)
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Data.Text.Lazy (Text)
import SkriptParser.Util (thd3, compareListLength)
import Text.Printf (printf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as T
import Data.Function (on)

type LoggerS = Logger -> Logger

data ErrorType = NoMatchErr | Malformed | Structure | Semantic | Except deriving (Eq, Ord, Show, Enum)
data ErrorContext = NoMatch | Matching | Initialization | ConstraintChecking | RestrictedSyntaxes
  deriving (Eq, Ord, Show, Enum)
data Verbosity = Low | Normal | High | VeryHigh | DebugVerbosity deriving (Eq, Ord, Show, Enum)

data LogEntryType = Info
             | Warning
             | Error ErrorType
             | Debug deriving (Eq, Ord, Show)
             
data LogEntry = LogEntry {
  message :: Text,
  tip :: Maybe Text,
  entryType :: LogEntryType,
  errCtx :: NonEmpty ErrorContext,
  fileContext :: Maybe (Text, Text, Int)
} deriving (Eq, Show)

data Logger = Logger {
  isDebug :: Bool,
  hasError :: Bool,
  fileInfo :: Maybe (Text, Seq Text, Int),
  errContext :: NonEmpty ErrorContext,
  entries :: [LogEntry],
  finalized :: [LogEntry]
}

errorMin :: LogEntry -> LogEntry -> Maybe LogEntry
errorMin e1 e2 = (\t1 t2 -> orderingMin e1 e2 (comparing errCtx e1 e2 <> compare t1 t2))
  <$> toErrorType e1
  <*> toErrorType e2
  where orderingMin :: a -> a -> Ordering -> a
        orderingMin _ y GT = y
        orderingMin x _ _  = x

formatLog :: Text -> Text -> Text -> Int -> Text
formatLog message fname line lineNum = T.pack $
  printf "%s (line %d: \"%s\", %s)" message lineNum line fname

makeLogEntry :: Text -> LogEntryType -> Maybe Text -> Logger -> LogEntry
makeLogEntry msg etype tip Logger { fileInfo, errContext } = case fileInfo of
  Just (fname, allLines, lineNum) ->
    let line = Seq.index allLines lineNum in
    LogEntry (formatLog msg fname line lineNum) tip etype errContext (Just (fname, line, lineNum))
  Nothing -> LogEntry msg tip etype errContext Nothing

toErrorType :: LogEntry -> Maybe ErrorType
toErrorType LogEntry {..} = case entryType of
  Error inf -> Just inf
  _ -> Nothing

emptyLogger :: Logger
emptyLogger = Logger False False Nothing (Matching :| []) [] []

switchContext :: ErrorContext -> LoggerS
switchContext newc l@Logger { errContext = _ :| r } = l { errContext = newc :| r }

finish :: Logger -> [LogEntry]
finish Logger {..} = -- Custom sortOn implementation
  map snd $
  sortBy (\ ~(y1,_) ~(y2,_) -> fromMaybe EQ (liftA2 compare y1 y2)) $
  map (\x -> (thd3 <$> fileContext x, x)) finalized

finalizeLogs :: LoggerS
finalizeLogs l@Logger {..} = l {
  finalized =
    let (bestErr, r) = foldr go (Nothing, []) entries in
    maybeToList bestErr ++ r
} where go :: LogEntry -> (Maybe LogEntry, [LogEntry]) -> (Maybe LogEntry, [LogEntry])
        go ent (e, nonErr) = case entryType ent of
          Error _ -> (minLog e ent, nonErr)
          _         -> (e, ent:nonErr)

        minLog :: Maybe LogEntry -> LogEntry -> Maybe LogEntry
        minLog ent e2 = ent >>= errorMin e2


-- Since we provide no way to manually edit errContext, this function is total
recurse :: Logger -> (Logger -> (a,Logger)) -> (a,Logger)
recurse logr f =
  let rec = logr { errContext = NE.cons Matching $ errContext logr } in
  let (a, logr') = f rec in
  (a, logr' { errContext = NE.fromList $ NE.drop 1 $ errContext logr' })

-- | Similar to @recurse@, but embeds the recursion mechanism within a state monad
--   Adds a level of recursion to the error context, runs the stateful operation, then jumps out of that recursion 
recurseState :: MonadState s m => (s -> Logger) -> (Logger -> s -> s) -> m a -> m a
recurseState extract embed m = do
  logr <- gets extract
  modify $ embed $ logr { errContext = NE.cons Matching $ errContext logr }
  res <- m
  logr' <- gets extract
  modify $ embed $ logr { errContext = NE.fromList $ NE.drop 1 $ errContext logr' }
  return res

nextLine :: LoggerS
nextLine l@Logger { fileInfo } = l { fileInfo = (succ <$>) <$> fileInfo }

forgetError :: LoggerS
forgetError l = l { hasError = False }

clearLessThanError :: LoggerS
clearLessThanError l@Logger { errContext, entries } = l { entries =
  filter (liftA2 (&&) 
    ((<= EQ) . (compareListLength `on` NE.tail) errContext . errCtx) 
    ((> Warning) . entryType)
  ) entries
}

clearErrors :: LoggerS
clearErrors l@Logger { entries } = l { entries = filter (isNothing . toErrorType) entries}

clearNotDebug :: LoggerS
clearNotDebug l@Logger { entries } = l { entries = filter ((>= Debug) . entryType) entries}

logNewEntry :: Text -> LogEntryType -> Maybe Text -> LoggerS
logNewEntry msg etype tip l@Logger { entries } =
  let newEnts = makeLogEntry msg etype tip l : entries in
  case etype of
    Error _ -> l { entries = newEnts, hasError = True }
    _       -> l { entries = newEnts }

logError :: Text -> ErrorType -> Maybe Text -> LoggerS
logError msg errtype tip l@Logger { hasError } = if not hasError
  then logNewEntry msg (Error errtype) tip (clearLessThanError l)
  else l

logWarn :: Text -> Maybe Text -> LoggerS
logWarn msg = logNewEntry msg Warning

logInfo :: Text -> LoggerS
logInfo msg = logNewEntry msg Info Nothing

logDebug :: Text -> LoggerS
logDebug msg l@Logger { isDebug } = if isDebug 
  then logNewEntry msg Debug Nothing l 
  else l