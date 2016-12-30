module C.Parse where

import C.Def
import Control.Monad
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

lexeme = (*>) (many spaceChar)

line = manyTill anyChar newline <?> "line"

ptr = many $ char '*'

parens = between (char '(') (char ')')

sep = oneOf ";,("

valid = some $ lowerChar <|> char '_'

identifier = label "identifier" $ lexeme $ (++) <$> ptr <*> valid

ctype = label "type" $ lexeme $ valid

parseCVar = label "variable" $ liftA2 CVar (many $ try ctype <* notFollowedBy sep) identifier

parseComment =
  label "comment" $
  TlComment <$>
  do string "/*"
     ret <- many $ notFollowedBy (string "*/") *> anyChar
     string "*/"
     return ret

parsePreproc = TlPreproc <$> (char '#' *> line)

parseCFunc =
  label "c function" $
  TlFunc <$>
  (CFunc <$> parseCVar <*> (parens $ many parseCVar) <*> (lexeme (char '{') *> many parseCVar))

parseTopLevel :: Parser TopLevelToken
parseTopLevel =
  label "top level" $ dbg "toplvl" $ (lexeme (parseComment <|> parsePreproc <|> parseCFunc))
