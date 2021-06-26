{-# LANGUAGE OverloadedStrings #-}
module SkriptParser.Matching.PatternParsing where

import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust, fromMaybe)
import Data.Sequence (Seq((:<|)))
import Data.Text.Lazy (Text)
import Data.Void (Void)
import SkriptParser.Logging (Logger, logError, ErrorType(..))
import SkriptParser.Matching.PatternMatching (PatternElement(..), Acceptance(..), ParseMark)
import SkriptParser.Util (always, todo)
import SkriptParser.Util.FState (FailState, justFail)
import SkriptParser.Util.Strings (splitOutsideParens)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Text.Lazy as T
import qualified Data.Set as S
import qualified Data.Sequence as Seq

type Parser = Parsec Void Text


parsePattern :: Text -> FailState Logger PatternElement
parsePattern str = case splitOutsideParens (== '|') str of -- Sequence cannot be empty here
  (s :<| Seq.Empty) -> handle $ runParser readPatternElem "" s
  _                 -> handle $ runParser readChoiceGroup "" ('(' `T.cons` str `T.snoc` ')')
  where handle :: Either (ParseErrorBundle Text Void) PatternElement -> FailState Logger PatternElement
        handle (Left (ParseErrorBundle (e :| _) _)) = 
          justFail (logError (T.pack $ parseErrorPretty e) Malformed Nothing)
        handle (Right e) = return e

readPatternElem :: Parser PatternElement
readPatternElem = mconcat <$> many (readText <|> readChoiceGroup <|> readOptional <|> readRegex <|> readExpression)
-- None are parsed, result is mempty == TextElement ""
-- One is parsed, result is that (<> doesn't create a CompoundElement for one element)
-- More are parsed, result is CompoundElement

readText :: Parser PatternElement
readText = do res <- takeWhile1P Nothing (not . isSpecial)
              next <- optional (satisfy isSpecial)
              case next of
                Just '\\' ->
                  do next' <- anySingle <?> "escaped character"
                     (TextElement (T.singleton next') <>) <$> readText
                Just ']' -> unexpected (Label "closing square bracket")
                Just '>' -> unexpected (Label "closing angle bracket")
                Just ')' -> unexpected (Label "closing parenthesis")
                _        -> return (TextElement res)

readChoiceGroup :: Parser PatternElement
readChoiceGroup = between (char '(') (char ')') $
  do choices <- sepBy1 readChoiceElement (char '|')
     if null $ tail choices
      then failure (Just $ Label "closing parenthesis") (S.singleton $ Label "at least 2 choices")
      else return $ ChoiceGroup choices

readChoiceElement :: Parser (PatternElement, ParseMark)
readChoiceElement = flip (,) <$> (fromMaybe 0 <$> optional (try (decimal <* char ':')))
                             <*> readPatternElem

readOptional :: Parser PatternElement
readOptional = OptionalGroup <$> between (char '[') (char ']') readPatternElem

readRegex :: Parser PatternElement
readRegex = RegexElement <$> between (char '<') (char '>') (takeWhile1P Nothing always)

readExpression :: Parser PatternElement
readExpression = do _ <- char '%'
                    nul <- isJust <$> optional (char '-')
                    cond <- isJust <$> optional (char '=')
                    types <- sepBy1 p (char '/')
                    if not $ any (todo "== booleanPatternType") types 
                      then unexpected (Label "'=' without any boolean pattern type")
                      else return (ExpressionElement types nul cond)
   where p = flip (,) <$> (fromMaybe All <$> optional readAcc) 
                      <*> (todo "parse pattern type" <$> takeWhile1P Nothing isAlpha)

readAcc :: Parser Acceptance
readAcc = do a <- anySingle
             case a of
               '*' -> return OnlyLiterals
               '~' -> return OnlyExpressions
               '^' -> return OnlyVariables
               _   -> if isAlpha a then return All
                      else unexpected (Tokens (a :| []))

orSwapped :: Parser a -> Parser b -> Parser (a,b)
orSwapped p q = ((,) <$> p <*> q) <|> (flip (,) <$> q <*> p)

isSpecial :: Char -> Bool
isSpecial c = c `elem` ['[', ']', '(', ')', '<', '>', '%', '\\']