module Main where

import C.Parse
import Control.Monad
import Data.Either
import System.Environment
import System.IO
import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Data.ByteString as BS

data Opt
  = Inplace
  | Output
  deriving (Show)

processArgs :: [String] -> IO [Either Opt FilePath]
processArgs [] = return []
processArgs (full@('-':opt):xs) =
  case opt of
    "i" -> ((Left Inplace) :) <$> processArgs xs
    "o" -> ((Left Output) :) <$> processArgs xs
    _ -> do
      hPutStrLn stderr $ "unknown option " ++ full
      processArgs xs
processArgs (file:xs) = ((Right file) :) <$> processArgs xs

parseFile f = openFile f ReadMode >>= BS.hGetContents >>= parseTest (many parseTopLevel)

main :: IO ()
main = do
  (opts, files) <- getArgs >>= processArgs >>= return . partitionEithers
  forM_ files parseFile
  return ()
