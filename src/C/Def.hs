module C.Def where

import Data.Functor.Identity
import Text.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token as T

data CType = CType
  { typeName :: String
  , typePtrs :: Int
  }

data CVar = CVar
  { varType :: CType
  , varName :: String
  }

data CFunc = CFunc
  { funcRetType :: CType
  , funcArgs :: [CVar]
  , funcDeclas :: [CVar]
  }

data TopLevelToken
  = TlFunc CFunc
  | TlPreproc String

newtype TopLevel =
  TopLevel [TopLevelToken]

cdef :: GenLanguageDef String s Identity
cdef =
  LanguageDef
    "/*"
    "*/"
    "//"
    True
    (letter <|> char '_')
    (alphaNum <|> char '_')
    (oneOf "=?!%/*<>|&^-+~")
    (oneOf "<>=&|+-")
    ["if", "else", "while", "for", "return", "sizeof", "continue", "break"]
    [ "+"
    , "-"
    , "*"
    , "/"
    , "%"
    , "++"
    , "--"
    , "=="
    , "!="
    , "<"
    , ">"
    , ">="
    , "<="
    , "&&"
    , "||"
    , "!"
    , "&"
    , "|"
    , "~"
    , "^"
    , "~"
    , "<<"
    , ">>"
    , "="
    , "+="
    , "*="
    , "-="
    , "/="
    , "%="
    , "<<="
    , ">>="
    , "&="
    , "^="
    , "|="
    ]
    True

clex = T.makeTokenParser cdef

identifier = T.identifier clex

whiteSpace = T.whiteSpace clex

parens = T.parens clex

braces = T.braces clex
