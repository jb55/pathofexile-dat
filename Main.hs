
module Main where

import Prelude hiding (readFile)
import PathOfExile.Parsing.Dat
import Data.ByteString (readFile)
import Control.Applicative
import System.Environment (getArgs)
import Data.Serialize.Get (runGet, Get)
import Control.Monad (forM_)

run :: FilePath -> Get a -> IO (Either String a)
run x p = runGet p <$> readFile x

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: poe-dat <SomeFile.dat>" 
    xs -> do
      recs <- mapM (\f -> run f pDatText) xs
      forM_ recs $ \r ->
        case r of
          Left s     -> putStrLn s
          Right recs -> print recs
