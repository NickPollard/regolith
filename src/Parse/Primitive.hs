module Parse.Primitive where

  {-
     This module defines parsers for primitive types (e.g. Ints, Bools, Arrays)
     from Lua bytecode files. Specific Lua types are parsed in the `Parse`
     module
     -}

import Control.Monad (replicateM)
import Control.Monad.Reader (ask, lift)
import Data.Binary (Get, get)
import Data.Binary.Get (getWord32le, getWord32be, getWord8, getWord64le, getWord64be)
import qualified Data.Binary.Bits.Get as Bits
import Data.Int (Int32)
import Data.Word (Word8, Word64)

import Types (Endianness(..), Parser)

-- Read a C `int` type
readInt :: Endianness -> Get Int32
readInt LittleEndian = fromIntegral <$> getWord32le
readInt BigEndian = fromIntegral <$> getWord32be

readInt' :: Parser Int32
readInt' = do
  endianness <- ask
  lift $ readInt endianness

readByte :: Get Word8
readByte = getWord8

readByte' :: Parser Word8
readByte' = lift getWord8

readBool :: Get Bool
readBool = (/= 0) <$> readByte

readBool' :: Parser Bool
readBool' = lift readBool

type SizeT = Word64

readSizeT :: Get SizeT
readSizeT = getWord64le

readSizeT' :: Parser SizeT
readSizeT' = do
  endianness <- ask
  case endianness of
    BigEndian -> lift getWord64be
    LittleEndian -> lift getWord64le

getInt32be :: Int -> Bits.BitGet Int32
getInt32be n = fromIntegral <$> Bits.getWord32be n

-- Strings are serialized as ASCII prefixed with number of chars as an Int
readString :: Get (Maybe String)
readString = do
  n <- getWord8
  n' <- if (n == 0xFF) then readSizeT else return (fromIntegral n)
  if (n' == 0)
     then return Nothing
     -- Size is n-1, as 0 == null/missing (as opposed to empty)
     else Just <$> replicateM (fromIntegral $ n'-1) (get :: Get Char)

-- Vectors are implemented with an `int` length prefix
readVectorN :: Get a -> Get [a]
readVectorN g = do
  n <- readInt LittleEndian -- TODO
  replicateM (fromIntegral n) g

readN' :: Parser a -> Parser [a]
readN' p = do
  n <- readInt'
  replicateM (fromIntegral n) p
