{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lua.Instruction.Args where

import Data.Binary.Bits.Get (BitGet)
import qualified Data.Binary.Bits.Put as Bits
import qualified Data.Binary.Bits.Get as Bits
import Data.Int (Int32)
import Data.Word (Word32)

-- TODO - for now we only implement instructions for Lua 5.1

-- | Coproduct of supported Argument pack formats. The type and constructors are promoted to the
--   kind and type level respectively.
data ArgFmt = ABC  -- three args: A, B, C
            | ABx  -- two args: A, and Bx (extended)
            | AsBx -- two args; A and sBx (extended and signed)

-- | GADT whose constructors witness the `ArgFmt` type parameter, allowing us to retrieve it in
--   pattern matches
data Args :: ArgFmt -> * where
  FmtABC :: Args 'ABC
  FmtABx :: Args 'ABx
  FmtAsBx :: Args 'AsBx

-- | GADT whose constructors witness the `ArgFmt` type parameter, allowing us to retrieve it in
--   pattern matches, whilst also containing the required argument values
data ArgPack :: ArgFmt -> * where
  ArgsAsBx :: Word32 -> Int32 -> ArgPack 'AsBx
  ArgsABx :: Word32 -> Word32 -> ArgPack 'ABx
  ArgsABC :: Word32 -> Word32 -> Word32 -> ArgPack 'ABC

instance Show (ArgPack a) where
  show (ArgsAsBx a sbx) = show a <> " " <> show sbx
  show (ArgsABx a bx) = show a <> " " <> show bx
  show (ArgsABC a b c) = show a <> " " <> show b <> " " <> show c

class ToArgs (a :: ArgFmt) where
  sing :: Args a

instance ToArgs 'ABC where
  sing = FmtABC
instance ToArgs 'ABx where
  sing = FmtABx
instance ToArgs 'AsBx where
  sing = FmtAsBx

{-
class IsArg (a :: ArgFmt) where
  parseArg' :: Word32 -> ArgPack a

instance IsArg 'Ax where
  parseArg' = parseAx
instance IsArg 'ABx where
  parseArg' = parseABx
instance IsArg 'ABC where
  parseArg' = parseABC

parseAx :: Word32 -> ArgPack 'Ax
parseAx = error "NYI"

parseABx :: Word32 -> ArgPack 'ABx
parseABx = error "NYI"

parseABC :: Word32 -> ArgPack 'ABC
parseABC = error "NYI"

parseArg :: IsArg a => Word32 -> ArgPack a
parseArg d = parseArg' d

parseArgs' :: Args a -> Word32 -> ArgPack a
parseArgs' FmtAx = parseArg
parseArgs' FmtABx = parseArg
parseArgs' FmtABC = parseArg
-}

parseArgs :: Args a -> BitGet (ArgPack a)
--parseArgs FmtAx = ArgsAx <$> Bits.getWord32be 26 -- TODO confirm endianness
parseArgs FmtAsBx = ArgsAsBx <$> Bits.getWord32be 8 <*> getSbx -- TODO confirm endianness
parseArgs FmtABx = ArgsABx <$> Bits.getWord32be 8 <*> Bits.getWord32be 18 -- TODO confirm endianness
parseArgs FmtABC = do
  a <- Bits.getWord32be 8 -- TODO confirm endianness
  -- C comes before B in Lua, for no obvious reason
  c <- Bits.getWord32be 9
  b <- Bits.getWord32be 9
  return $ ArgsABC a b c

getSbx :: BitGet Int32
getSbx = do
  n <- Bits.getWord32be 18
  return $ (fromIntegral n) - max_sbx

max_sbx :: Int32
max_sbx = 131071 -- 2^17 - 1

writeSbx :: Int32 -> Bits.BitPut ()
writeSbx sbx = do
  Bits.putWord32be 18 . fromIntegral $ sbx + max_sbx

writeArgs :: ArgPack a -> Bits.BitPut ()
writeArgs (ArgsAsBx a sbx) = do
  writeSbx sbx
  Bits.putWord32be 8 a
writeArgs (ArgsABx a bx) = do
  Bits.putWord32be 18 bx
  Bits.putWord32be 8 a
writeArgs (ArgsABC a b c) = do
  Bits.putWord32be 9 b
  Bits.putWord32be 9 c
  Bits.putWord32be 8 a
