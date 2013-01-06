
module Main where

import Prelude hiding (readFile)
import PathOfExile.Parsing.Dat
import Data.ByteString (readFile)
import Control.Applicative
import System.Environment (getArgs)
import Data.Serialize.Get (runGet, Get)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Control.Lens
import Data.List (find, isSuffixOf)

run :: (MetaSize -> Get a) -> FilePath -> IO (Either String a)
run p fn = runGet (p $ getMr fn) <$> readFile fn

datInfo :: [(String, MetaSize)]
datInfo = mapped._2 %~ MetaSize $ [
    ("Maps", 24)
  , ("BackendError", 20)
  , ("BaseItemTypes", 144)
  ]

getMr f = fromMaybe (MetaSize 16) $ snd <$> find matchesName datInfo
  where
    matchesName (n, _) = (n ++ ".dat") `isSuffixOf` f

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: poe-dat <SomeFile.dat>" 
    xs -> do
      recs <- mapM (run pDatText) xs
      forM_ recs $ \r ->
        case r of
          Left s     -> putStrLn s
          Right recs -> mapM_ (print . recData) recs
