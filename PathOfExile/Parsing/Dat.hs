
module PathOfExile.Parsing.Dat (
  pDatText
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

data Record a = Record { recOffset  :: !Word32
                       , recSmallId :: !Word32
                       , recUnk1    :: !Word32
                       , recBigId   :: !Word32
                       , recData    :: a
                       } deriving (Show)

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

pMetaRecord :: Get (a -> Record a)
pMetaRecord = Record <$> w32 <*> w32 <*> w32 <*> w32

pDatByteStrings :: Get [Record ByteString]
pDatByteStrings = pDat pBSRecord

pDatFromBS :: (ByteString -> a) -> Get [Record a]
pDatFromBS f = pDat (f <$> pBSRecord)

pDatText :: Get [Record Text]
pDatText = pDatFromBS decodeUtf16LE

pDat :: Get a -> Get [Record a]
pDat p = do
  n   <- w32
  mrs <- replicateM (fromIntegral n) pMetaRecord
  skip 8
  mapM go mrs
  where
    go x = do
      rec <- p 
      return $ x rec

    

