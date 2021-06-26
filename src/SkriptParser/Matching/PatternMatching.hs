{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SkriptParser.Matching.PatternMatching where

import Prelude hiding (log)
  
import Control.Applicative (liftA2, (<|>))
import Control.Monad (MonadPlus(..), mfilter)
import Control.Monad.State (gets, get)
import Data.Bits ((.|.))
import Data.Char (isSpace, toLower)
import Data.Foldable (asum, toList)
import Data.Functor ((<&>))
import Data.Maybe (isJust, fromMaybe)
import Data.Sequence (Seq((:<|)), (><), (|>))
import Data.Text.Lazy (Text)
import SkriptParser.TriggerState (TriggerState)
import SkriptParser.Types (PatternType)
import SkriptParser.Util (Todo, todo)
import SkriptParser.Util.FState
import SkriptParser.Util.Strings
import Text.Printf (printf)
import Text.Regex.PCRE (Regex, MatchText, matchOnceText, makeRegex, matchAll)
import qualified Data.Array as A
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as T

type RegexMatch = [Text]

data MatchContext = MatchContext {
  leftToMatch :: Text,
  originalPattern :: Text,
  originalElem :: PatternElement,
  triggerState :: TriggerState,
  logger :: Todo,
  source :: Maybe MatchContext,
  parsedExpressions :: [Todo],
  regexMatches :: [RegexMatch],
  patternIndex :: Int,
  parseMark :: ParseMark
}

instance Show MatchContext where
  showsPrec _ MatchContext {..} =
    showString "MatchContext{leftToMatch = \"" .
    showText leftToMatch .
    showString "\", originalElement = " .
    shows originalElem .
    showChar '}'
  

emptyMatchContext :: MatchContext
emptyMatchContext = MatchContext "" todo todo todo todo Nothing [] [] 0 0

makeMatchContext :: Text -> PatternElement -> TriggerState -> Todo -> MatchContext
makeMatchContext toMatch e tstate logger = sourcedMatchContext toMatch e tstate logger Nothing

sourcedMatchContext :: Text -> PatternElement -> TriggerState -> Todo -> Maybe MatchContext -> MatchContext
sourcedMatchContext toMatch e tstate logger src = emptyMatchContext {
                                                    leftToMatch = toMatch,
                                                    originalPattern = todo,
                                                    originalElem = e,
                                                    triggerState = tstate,
                                                    logger = logger,
                                                    source = src
                                                  }

infixl 3 +#

(+#) :: MatchContext -> Todo -> MatchContext
mc +# expr = mc { parsedExpressions = expr : parsedExpressions mc }

infixl 3 +&

(+&) :: MatchContext -> RegexMatch -> MatchContext
mc +& reg = mc { regexMatches = reg : regexMatches mc }

infixl 3 +|

(+|) :: MatchContext -> ParseMark -> MatchContext
mc +| mark = mc { parseMark = parseMark mc .|. mark}

infixl 3 +>

(+>) :: MatchContext -> Todo -> MatchContext
mc +> log = mc { logger = log }

advance :: MatchContext -> MatchContext
advance mc = mc { patternIndex = patternIndex mc + 1 }

branch :: MatchContext -> PatternElement -> MatchContext
branch src e = sourcedMatchContext (leftToMatch src) e (triggerState src) (logger src) (Just src)

(>+<) :: MatchContext -> MatchContext -> MatchContext
mc >+< branch = mc {
  parsedExpressions = parsedExpressions branch ++ parsedExpressions mc,
  regexMatches = regexMatches branch ++ regexMatches mc,
  parseMark = parseMark mc .|. parseMark branch
}

logMap :: (Todo -> Todo) -> MatchContext -> MatchContext
logMap f ctx = ctx { logger = f $ logger ctx }

type ParseMark = Int
data Acceptance = All | OnlyLiterals | OnlyExpressions | OnlyVariables deriving (Eq, Show, Enum)

showAcc :: Acceptance -> Text
showAcc All = ""
showAcc OnlyLiterals = "*"
showAcc OnlyExpressions = "~"
showAcc OnlyVariables = "^"

data PatternElement = ChoiceGroup [(PatternElement, ParseMark)]
                    | CompoundElement [PatternElement]
                    | ExpressionElement [(PatternType, Acceptance)] Bool Bool
                    | OptionalGroup PatternElement
                    | TextElement Text
                    | RegexElement Text
  deriving (Eq)

instance Show PatternElement where
  showsPrec _ (TextElement str) = showText str
  showsPrec _ (RegexElement reg) = showChar '<' . showText reg . showChar '>'
  showsPrec _ (OptionalGroup e) = showChar '[' . shows e . showChar ']'
  showsPrec _ (CompoundElement elems) = foldr ((.) . shows) id elems
  showsPrec _ (ChoiceGroup choices) = showParen True $ go choices
    where go [] = id
          go [(e,m)]
            | m == 0 = shows e
            | otherwise = shows m . showChar ':' . shows e
          go ((e,m):r)
            | m == 0 = shows e . showChar '|' . go r
            | otherwise = shows m . showChar ':' . shows e . showChar '|' . go r
  showsPrec _ (ExpressionElement types nul cond) =
      showChar '%' .
      (if nul then showChar '-' else id) .
      (if cond then showChar '=' else id) .
      go types
    where go [] = errorWithoutStackTrace "Empty ExpressionElement"
          go [(t,a)] = showText (showAcc a) . shows t
          go ((t,a):r) = showText (showAcc a) . shows t . showChar '/' . go r

instance Semigroup PatternElement where
  a <> TextElement "" = a
  TextElement "" <> a = a
  TextElement s1 <> TextElement s2 = TextElement (s1 `T.append` s2)
  CompoundElement es <> a = CompoundElement (es ++ [a]) 
  -- Needed to make the operation associative, but please just use it right-associatively
  a <> CompoundElement es = CompoundElement (a:es)
  a <> b = CompoundElement [a,b]

instance Monoid PatternElement where
  mempty = TextElement ""

endOfLine :: Text
endOfLine = "\0"

flatten :: PatternElement -> [PatternElement]
flatten (CompoundElement elems) = elems
flatten e                       = [e]

initLookahead :: [PatternElement] -> Int -> Maybe MatchContext -> (Seq PatternElement, [PatternElement])
initLookahead flat patIndex src
  | (Just src') <- src,
    patIndex + 1 >= length flat = initLookahead (flatten $ originalElem src') (patternIndex src') (source src')
  | otherwise                   = (possibleInputs $ drop (patIndex + 1) flat, flat)

possibleInputs :: [PatternElement] -> Seq PatternElement
possibleInputs = go Seq.empty Seq.empty
  where go optionalPoss poss []          = (poss >< optionalPoss) |> TextElement endOfLine
        go optionalPoss poss (e:r)
          | TextElement str <- e       = let blank = T.all isSpace str in
                                         if T.null str || blank && null r then poss
                                         else if blank then go optionalPoss poss r
                                         else (poss |> e) >< optionalPoss
          | ChoiceGroup choices <- e   = foldl ((. possibleInputs . flatten) . (><)) poss (map fst choices)
                                         >< optionalPoss
          | OptionalGroup element <- e = go (optionalPoss >< possibleInputs (flatten element)) poss r
          | otherwise                  = (poss |> e) >< optionalPoss

match :: PatternElement -> SelfFailState MatchContext
match e = mapFState (\(_,ctx) -> (mfilter (T.null . leftToMatch) (Just ctx), ctx)) (match' e)
  where match' :: PatternElement -> SelfFailState MatchContext
        match' (TextElement str) = matchText str
        match' (RegexElement regex) = matchRegex regex
        match' (CompoundElement elems) = mfilter (liftA2 (||) (isJust . source) (T.null . leftToMatch)) $
          mconcat (match' <$> elems)
        match' (OptionalGroup element) =
          do bran <- gets (`branch` element)
             (>+< bran) <$> (withFState (const bran) (match' element) <|> mempty)
        match' (ChoiceGroup choices) = uncurry (+|) <$> asum (map checkChoice choices)
            where checkChoice :: (PatternElement, ParseMark) -> FailState MatchContext (MatchContext, ParseMark)
                  checkChoice (element, pm) = do bran <- gets (`branch` element)
                                                 (,pm) . (>+< bran) <$> withFState (const bran) (match' element)
        match' (ExpressionElement types nullable conditional) = matchExprElem types nullable conditional
                                            

matchText :: Text -> SelfFailState MatchContext
matchText matchAgainst
  | T.null matchAgainst = mempty
  | otherwise = do
    ctx <- get
    maybe mzero (\(rest,_) -> success $ ctx { leftToMatch = T.dropWhile isSpace rest }) $
      foldlMText matchWithSpaces (leftToMatch ctx, False) matchAgainst
  where matchWithSpaces s@(Empty, b) c
          | b && isSpace c = Just s -- If it's whitespace at the end of matchAgainst it doesn't count
          | otherwise      = Nothing -- Otherwise, abort
        matchWithSpaces (str@(h :> r), whitespaceSearch) c
          | isSpace h && isSpace c = matchWithSpaces (r, True) c
          -- As long as c is whitespace, we consume as much of it in str as possible, and trigger whitespace search
          | isSpace h              = matchWithSpaces (r, whitespaceSearch) c
          -- Disregard a space at the start of str otherwise
          | isSpace c              = Just (str, True)
          -- We consumed as much whitespace as possible in str, yet there is still whitespace in matchAgainst, so we
          -- consume that too.
          | otherwise              = if toLower c == toLower h then Just (r, False) else Nothing
          -- If h and c are the same, continue, otherwise fail. At this point, neither h nor c are whitespace, so we
          -- turn off whitespace search if it was still on

matchRegex :: Text -> SelfFailState MatchContext
matchRegex regex = do ctx <- get
                      let reg = makeCIRegex ('^' `T.cons` regex) -- We only want matches from the start
                      let (possInputs,_) = initLookahead (flatten $ originalElem ctx) (patternIndex ctx) (source ctx)
                      iterPos reg possInputs
  where iterPos :: Regex -> Seq PatternElement -> SelfFailState MatchContext
        iterPos _ Seq.Empty = mzero
        iterPos reg (pos :<| r)
          | (TextElement str) <- pos, not (T.null str) = (
              do ltm <- gets leftToMatch
                 let matchAgainst = fromMaybe "" $
                      if str == endOfLine then Just ltm
                      else substringBefore str ltm
                 let res = matchOnceText (reg :: Regex) (T.unpack matchAgainst) :: Maybe (String, MatchText String, String)
                 maybeFState res $ \(_,mtch,_) ctx ->
                    ctx {
                        leftToMatch = T.drop (fromIntegral $ snd $ snd (mtch A.! 0)) (leftToMatch ctx)
                    } +& map (T.pack . fst) (A.elems mtch)
            ) <|> iterPos reg r
          | (RegexElement pat) <- pos = (
              do let reg2 = makeRegex (T.unpack pat) :: Regex
                 ltm <- gets (T.unpack . leftToMatch)
                 let bounds = map (fromIntegral . snd . (A.! 0)) $ matchAll reg2 ltm
                 let matchres = asum $ map (\b -> (b,) <$> matchOnceText reg (take (fromIntegral b) ltm)) bounds
                 maybeFState matchres $ \(bound, (_,match1,_)) ctx ->
                    ctx {
                      leftToMatch = T.drop bound (T.pack ltm)
                    } +& map (T.pack . fst) (A.elems match1)
            ) <|> iterPos reg r
          | otherwise                 = iterPos reg r


matchExprElem :: [(PatternType, Acceptance)] -> Bool -> Bool -> SelfFailState MatchContext
matchExprElem types _ conditional =
  do ctx <- mempty
     let log = logger ctx
     let (possInputs, flat) = initLookahead (flatten $ originalElem ctx) (patternIndex ctx) (source ctx)
     iter flat log possInputs
  where iter :: [PatternElement] -> Todo -> Seq PatternElement -> SelfFailState MatchContext
        iter _ _  Seq.Empty = mzero
        iter flat log (pos :<| r)
          | (TextElement str) <- pos, not (T.null str) = (do
            (ltm, pindex) <- gets2 leftToMatch patternIndex
            if str == endOfLine then
              if pindex == 0 then mzero
              else do let toParse = T.dropWhile isSpace ltm
                      handleParsed ((,toParse) <$> parse toParse)
            else do let toParses = allSubstringsBefore str ltm
                    handleParsed (parse `untilSuccessWithInput` (T.dropWhile isSpace <$> toParses))
            ) <|> iter flat log r
          | (RegexElement pat) <- pos = (
                do (ltm, op) <- gets2 leftToMatch originalPattern
                   let bounds = map (fromIntegral . snd . (A.! 0)) $ 
                                matchAll (makeRegex (T.unpack pat) :: Regex) (T.unpack ltm)
                   let toParses = filter ((== LT) . flip T.compareLength (fromIntegral $ T.length op)) $
                                  map (`T.take` ltm) bounds
                   handleParsed (parse `untilSuccessWithInput` toParses)
              ) <|> iter flat log r
          | ExpressionElement {} <- pos = (
              let nextPossInputs = possibleInputs (toList r) in
              if any (\case {(TextElement _) -> False ; _ -> True}) nextPossInputs then mzero
              else iter' $ (\case {(TextElement t) -> t ; _ -> errorWithoutStackTrace ""}) <$> nextPossInputs
            ) <|> iter flat log r
          | otherwise = iter flat log r

        iter' :: Seq Text -> SelfFailState MatchContext
        iter' poss = asum $ poss <&> \pos ->
            do ltm <- gets leftToMatch
               let matchAgainst = fromMaybe "" $
                    if pos == endOfLine then Just ltm
                    else substringBefore pos ltm
               handleParsed (parse `untilSuccessWithInput` splitOutsideParens isSpace matchAgainst)

        parse :: Text -> FailState MatchContext Todo
        parse toParse = asum (makeState <$> types)
          where makeState :: (PatternType, Acceptance) -> FailState MatchContext Todo
                makeState (typ, acc) = do
                    let doParse = withFState (todo `logMap`)
                          (if todo typ {- == booleanType -} then todo toParse conditional else todo toParse typ)
                    checkAcceptance acc toParse doParse

        checkAcceptance :: Acceptance -> Text -> FailState MatchContext Todo -> FailState MatchContext Todo
        checkAcceptance acceptance s m = do
          (expr,mc) <- packFState m
          let log = logger mc
          case acceptance of
            All -> m
            OnlyExpressions ->
              if todo expr then 
                thenFail (+> todo log (printf "Only expressions are allowed, found literal '%s'" s :: String) todo) m
              else m
            OnlyLiterals ->
              if not (todo expr) then 
                thenFail (+> todo log (printf "Only literals are allowed, found expression '%s'" s :: String) todo) m
              else m
            OnlyVariables ->
              if todo expr then 
                thenFail (+> todo log (printf "Only variables are allowed, found '%s'" s :: String) todo) m
              else m

        handleParsed :: FailState MatchContext (Todo, Text) -> SelfFailState MatchContext
        handleParsed = mapSFState (\((expr,parsed),mc) ->
                          mc { leftToMatch = T.drop (fromIntegral $ T.length parsed) (leftToMatch mc) } +# expr
                       )

