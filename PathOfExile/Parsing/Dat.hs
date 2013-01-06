{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PathOfExile.Parsing.Dat (
  pDatText
, MetaSize(..)
, Record(..)
) where

import Data.Serialize.Get
import Blaze.ByteString.Builder.Word (fromWord8s)
import Data.ByteString
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Word (Word32)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf16LE)
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Word (fromWord16shost)
import Debug.Trace (trace)

data Record a = Record { recMetaData :: ByteString
                       , recData     :: a
                       } deriving (Show)

newtype MetaSize = MetaSize { getMetaSize :: Int }
                 deriving (Show, Eq, Ord, Num)

type DatState = ()

w32 :: Get Word32
w32 = getWord32host

manyTill :: (Alternative f) => f a -> f b -> f [a]
manyTill p end = scan
  where scan = (end *> pure []) <|> liftA2 (:) p scan

satisfy :: (a -> Bool) -> String -> Get a -> Get a
satisfy pred msg g = do
  x <- g
  if pred x then return x else fail msg

nullWord32 :: Get Word32
nullWord32 = satisfy (==0) "Failed to parse null Word32" w32

pBSRecord :: Get ByteString
pBSRecord = do
  words <- manyTill getWord16host (lookAhead nullWord32)
  skip 4
  return $ toByteString $ fromWord16shost words

pMetaRecord :: MetaSize -> Get (a -> Record a)
pMetaRecord (MetaSize n) = Record <$> getByteString n

pDatFromBS :: MetaSize -> (ByteString -> a) -> Get [Record a]
pDatFromBS n f = pDat (f <$> pBSRecord) n

pDatText :: MetaSize -> Get [Record Text]
pDatText n = pDatFromBS n decodeUtf16LE

pDat :: Get a -> MetaSize -> Get [Record a]
pDat p mrsize = do
  n   <- w32
  mrs <- replicateM (fromIntegral n) (pMetaRecord mrsize)
  skip 8
  mapM go mrs
  where
    go x = do
      rec <- p 
      return $ x rec

