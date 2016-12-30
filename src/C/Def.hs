module C.Def where

data CVar = CVar
  { varType :: [String]
  , varName :: String
  } deriving (Show)

data CFunc = CFunc
  { funcRet :: CVar
  , funcArgs :: [CVar]
  , funcDeclas :: [CVar]
  } deriving (Show)

data TopLevelToken
  = TlFunc CFunc
  | TlComment String
  | TlPreproc String
  deriving (Show)
