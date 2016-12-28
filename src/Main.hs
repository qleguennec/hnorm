module Main where

import C.Def
import C.Parse
import Data.Either
import System.Environment
import System.IO

data Opt = Inplace | Output
  deriving Show

processArgs :: [String] -> IO [Either Opt (FilePath, (IO Handle))]
processArgs [] = return []
processArgs (full@('-':opt):xs) =
  case opt of
    "i" -> ((Left Inplace) :) <$> processArgs xs
    "o" -> ((Left Output) :) <$> processArgs xs
    _ -> do
      hPutStrLn stderr $ "unknown option " ++ full
      processArgs xs
processArgs (file:xs) = ((Right (file, openFile file ReadMode)) :) <$> processArgs xs

main :: IO ()
main = do
  (opts, files) <- getArgs >>= processArgs >>= return . partitionEithers
  sequence_ $ map (putStrLn . show) opts
