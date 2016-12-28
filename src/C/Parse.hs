module C.Parse where

import C.Def
import Text.Parsec
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

parsePtrs = length <$> many (char '*')

parseType = do
  typename <- identifier
  whiteSpace
  typeptrs <- parsePtrs
  return $ CType typename typeptrs

parseVar = do
  vartype <- parseType
  whiteSpace
  varname <- identifier
  return $ CVar vartype varname

parseCPreproc = char '#' >> TlPreproc <$> identifier

parseCFunc = do
  functype <- parseType
  whiteSpace
  args <- parens (many parseVar)
  char '{'
  declas <- many parseVar
  return $ TlFunc $ CFunc functype args declas

parseTopLevel :: Parser TopLevelToken
parseTopLevel = whiteSpace >> (parseCFunc <|> parseCPreproc)
