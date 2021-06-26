{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SkriptParser.Util.Strings (
  allSubstringsBefore,
  substringBefore,
  makeCIRegex,
  getEnclosed,
  splitOutsideParens,
  showText,
  foldlMText,

  pattern (:>),
  pattern Empty
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq, (|>))
import Data.String (IsString(..))
import Data.Text.Lazy (Text)
import Text.Regex.PCRE (Regex, makeRegexOpts, compCaseless, defaultExecOpt)
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as T

pattern (:>) :: Char -> Text -> Text
pattern x :> xs <- (T.uncons -> Just (x, xs))

pattern Empty :: Text
pattern Empty <- (T.uncons -> Nothing)

showText :: Text -> ShowS
showText = showString . T.unpack

allSubstringsBefore :: Text -> Text -> [Text]
allSubstringsBefore before takeFrom = filter (T.isPrefixOf (T.toLower before)) (T.tails (T.toLower takeFrom))

substringBefore :: Text -> Text -> Maybe Text
substringBefore = (listToMaybe .) . allSubstringsBefore

-- | Compile a string into a case-insensitive regex
makeCIRegex :: Text -> Regex
makeCIRegex = makeRegexOpts compCaseless defaultExecOpt . T.unpack

-- | Returns the text enclosed between the opening and closing characters (first and second argument respectively)
--   plus the rest of the given text, or Nothing. The given text must start with the opening character, otherwise
--   Nothing is returned.
getEnclosed :: Text -> Char -> Char -> Maybe (Text, Text)
getEnclosed s open close = if T.head s /= open then Nothing else go 0 T.empty s
  where go :: Int -> Text -> Text -> Maybe (Text, Text)
        go _ _ Empty = Nothing
        go d b (c :> r)
          | d == 1 && c == close = Just (r, T.reverse b)
          | c == open  = if d == 0 then go (succ d) b r
                         else go (succ d) (c `T.cons` b) r
          | c == close = go (pred d) (c `T.cons` b) r
          | otherwise  = go d (c `T.cons` b) r

-- | Split a @Text@ whenever the given character predicate is satisfied outside of a group of parentheses.
--   Never empty.
splitOutsideParens :: (Char -> Bool) -> Text -> Seq Text
splitOutsideParens p = go (Seq.empty, "")
  where go :: (Seq Text, Text) -> Text -> Seq Text
        go (spl, Empty) Empty = spl
        go (spl, buf) Empty = spl |> T.reverse buf
        go (spl, buf) s@('(' :> r) = case getEnclosed s '(' ')' of
            Nothing -> go (spl, '(' `T.cons` buf) r
            Just (rest, enclosed) -> go (spl, (')' `T.cons` T.reverse enclosed) `T.append` ('(' `T.cons` buf)) rest
        go (spl, buf) (c :> r)
          | T.null buf && p c = go (spl, "") r
          | p c = go (spl |> T.reverse buf, "") r
          | otherwise = go (spl, c `T.cons` buf) r
        
foldlMText :: Monad m => (b -> Char -> m b) -> b -> Text -> m b
foldlMText f z0 xs = T.foldr c return xs z0
  -- See Note [List fusion and continuations in 'c']
  where c x k z = f z x >>= k
        {-# INLINE c #-}
        
instance IsString (NonEmpty Char) where
  fromString = NE.fromList