module C.Output where

import C.Def
import Data.List
import System.IO

ptrs = flip replicate '*'

space = intercalate " "

putFArgs :: Handle -> [CVar] -> IO ()
putFArgs h [] = hPutStrLn h "(void)"
putFArgs h ((CVar (CType tn tp) vn):args) = do
  hPutStr h $ '\t' : '\t' : '(' : ((space tn) ++ " " ++ (ptrs tp) ++ vn)
  putRemaining args
  where
    putRemaining [] = hPutStrLn h ")"
    putRemaining ((CVar (CType tn tp) vn):args) =
      hPutStr h $ "\n\t\t, " ++ (space tn) ++ (ptrs tp) ++ vn

writeTopLevel :: Handle -> TopLevelToken -> IO ()
writeTopLevel h (TlPreproc s) = hPutStrLn h $ '#' : s
writeTopLevel h (TlFunc (CFunc (CVar (CType tn tp) n) cvars declas)) =
  let put = hPutStrLn h
  in do put (space tn)
        put $ ('\t' : (ptrs tp)) ++ (space tn)
        putFArgs h cvars
