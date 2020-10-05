{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lua.Instruction where

import Data.Binary.Get (Get)
import Data.Binary.Put
import Data.Binary.Bits.Put (runBitPut)
import qualified Data.Binary.Bits.Put as Bits
import qualified Data.Binary.Bits.Get as Bits
import Data.Binary.Bits.Get (runBitGet, BitGet)
import Data.Word (Word8)

import Lua.Instruction.Args
import Lua.Instruction.TH (mkOpArgss, OpCode'(..))
import Lua.Types (TValue)
import Lua.Version (Version(..), V(..))

-- TODO - we should use the C-Preprocessor to generate this file (and avoid repetition)
-- TODO - can we simplify the reflection and translation between Ops and Args?
-- TODO - or use Template Haskell instead

class VersionedInst (v :: Version) where
  type VInstruction v

instance VersionedInst 'Lua51 where
  type VInstruction 'Lua51 = SomeInst

instance VersionedInst 'Lua53 where
  type VInstruction 'Lua53 = SomeInst

  {-
#define GEN_WITH_OPCODES(f) \
  f(MOVE, Move, 0, ABC) \
  f(LOADK, LoadK, 1, ABx)

data Opcode where
#define MK_DATA(op, pretty, code, args) op :: Opcode
GEN_WITH_OPCODES(MK_DATA)

data Op :: OpCode -> * where
#define MK_GADT(op, pretty, code, args) Op##pretty :: Op 'OP_##op
GEN_WITH_OPCODES(MK_GADT)

instance Show (Op o) where
#define MK_SHOW(op, pretty, code, args) show Op#pretty = ##op
GEN_WITH_OPCODES(MK_SHOW)

-- The OpCode type implies the ArgFmt type
type family OpArgs (o :: OpCode) :: ArgFmt
#define MK_OP_ARGS(op, pretty, code, args)   type instance OpArgs 'OP_##op = '##args
GEN_WITH_OPCODES(MK_OP_ARGS)

argFormat :: Op o -> Args (OpArgs o)
#define MK_ARG_FORMAT(op, pretty, code, args) argFormat Op##pretty = sing
GEN_WITH_OPCODES(MK_ARG_FORMAT)

opCode :: Op o -> Word8
#define MK_OP_CODE(op, pretty, code, args) opCode Op##pretty = code
GEN_WITH_OPCODES(MK_ARG_FORMAT)
-}

-- | Coproduct of supported OpCodes. The type and constructors are promoted to the kind and type
--   level respectively.
data OpCode
  = OP_MOVE
  | OP_LOADK
  | OP_LOADBOOL
  | OP_LOADNIL
  | OP_CALL
  | OP_RETURN
  -- ...
  {-
OP_SELF
OP_ADD
OP_SUB
OP_MUL
OP_DIV
OP_MOD
OP_POW
OP_UNM
OP_NOT
OP_LEN
OP_CONCAT
OP_JMP
OP_EQ
OP_LT
OP_LE
OP_TEST
OP_TESTSET
OP_CALL
OP_TAILCALL
OP_RETURN
OP_FORLOOP
OP_FORPREP
OP_TFORLOOP
OP_SETLIST
OP_CLOSE
OP_CLOSURE
OP_VARARG
-}

-- | GADT whose constructors witness the `OpCode` type parameter, allowing us to retrieve it in
--   pattern matches
data Op :: OpCode -> * where
  OpMove   :: Op 'OP_MOVE
  OpLoadK  :: Op 'OP_LOADK
  OpLoadBool :: Op 'OP_LOADBOOL
  OpLoadNil :: Op 'OP_LOADNIL
  OpCall   :: Op 'OP_CALL
  OpReturn :: Op 'OP_RETURN

instance Show (Op o) where
  show OpMove = "MOVE"
  show OpCall = "CALL"
  show OpReturn = "RETURN"
  show OpLoadK = "LOADK"
  show OpLoadBool = "LOADBOOL"
  show OpLoadNil = "LOADNIL"

data SomeOp where
  SomeOp :: Op o -> SomeOp

-- | Tag an OpCode-indexed type with a GADT witness whilst erasing the type. The GADT witness
--   allows us to later recall the OpCode when pattern matching
data OpCoded (f :: OpCode -> *) where
  OpCoded :: Op v -> f v -> OpCoded f

withSomeOp :: SomeOp -> (forall o. Op o -> r) -> r
withSomeOp (SomeOp op) f = f op

overSomeOp :: (forall o. Op o -> r) -> SomeOp -> r
overSomeOp f (SomeOp op) = f op

reflectOp :: OpCode -> SomeOp
reflectOp OP_MOVE = SomeOp OpMove
reflectOp OP_LOADK = SomeOp OpLoadK
reflectOp OP_LOADBOOL = SomeOp OpLoadBool
reflectOp OP_LOADNIL = SomeOp OpLoadNil
reflectOp OP_CALL = SomeOp OpCall
reflectOp OP_RETURN = SomeOp OpReturn

-- The OpCode type implies the ArgFmt type
type family OpArgs (o :: OpCode) :: ArgFmt

type instance OpArgs 'OP_MOVE = 'ABC
type instance OpArgs 'OP_LOADK = 'ABx
type instance OpArgs 'OP_LOADBOOL = 'ABC
type instance OpArgs 'OP_LOADNIL = 'ABC
type instance OpArgs 'OP_CALL = 'ABC
type instance OpArgs 'OP_RETURN = 'ABC

-- TODO(nickpollard) - Take into account version
-- | An Instruction whose OpCode is signified by the type parameter `o`
data Instruction (o :: OpCode) where
  Instruction :: ArgPack (OpArgs o) -> Instruction o

-- TODO(nickpollard) - Take into account version
type SomeInst = OpCoded Instruction

class IsInst (o :: OpCode) where
  instArgs :: Op o -> Args (OpArgs o)

--op_return :: Instruction 'OP_RETURN
--op_return = Instruction (ArgsABC 1 2 3)

--op_call :: Instruction 'OP_CALL
--op_call = Instruction (ArgsABC 1 2 3)

--op_loadk :: Instruction 'OP_LOADK
--op_loadk = Instruction (ArgsABx 1 2)

parseOp :: Word8 -> Maybe OpCode
parseOp op = case op of
  0 -> Just OP_MOVE
  1 -> Just OP_LOADK
  36 -> Just OP_CALL
  38 -> Just OP_RETURN
  _ -> Nothing

parseOp' :: Word8 -> Maybe SomeOp
parseOp' op = reflectOp <$> parseOp op

argFormat :: Op o -> Args (OpArgs o)
argFormat OpMove = sing
argFormat OpLoadK = sing
argFormat OpLoadBool = sing
argFormat OpLoadNil = sing
argFormat OpCall = sing
argFormat OpReturn = sing

readInstruction :: V v -> Get (VInstruction v)
readInstruction V51 = readInstruction'
readInstruction V53 = readInstruction'

-- Read 32 bits from a binary stream as a complete Lua instruction
readInstruction' :: Get SomeInst
readInstruction' = runBitGet $ do
  -- The Instruction is the lower (? TODO) 6 bits
  op <- Bits.getWord8 6
  -- The Args are in the remaining 26 bits, with a format determined by the opcode
  maybe (error "Failed to parse instruction; Invalid OpCode") (overSomeOp readOpArgs) $ parseOp' op
  where readOpArgs :: Op o -> BitGet SomeInst
        readOpArgs opcode = OpCoded opcode . Instruction <$> parseArgs (argFormat opcode)

-- Write a Lua Instruction as a 32-bit value
writeInstruction :: SomeInst -> Put
writeInstruction (OpCoded op (Instruction args)) = runBitPut $ do
  writeOp op
  writeArgs args

writeOp :: Op o -> Bits.BitPut ()
writeOp = Bits.putWord8 6 . opCode

opCode :: Op o -> Word8
opCode op = case op of
  OpMove -> 0
  OpLoadK -> 1
  OpLoadBool -> 2
  OpLoadNil -> 3
  OpCall -> 36
  OpReturn -> 38

printInst :: V v -> [Maybe String] -> [TValue] -> VInstruction v -> String
printInst V51 upvalues consts = printInst' upvalues consts
printInst V53 upvalues consts = printInst' upvalues consts

printInst' :: [Maybe String] -> [TValue] -> SomeInst -> String
printInst' _ _ (OpCoded op (Instruction args)) =
  pad asm width <> comment
    where asm = show op <> " " <> show args
          comment = "" --runReader (commentInst upvalues inst) _consts
          width = 30
          pad msg w = msg <> (replicate (w - length msg) ' ')

$(mkOpArgss [
  ("OP__MOVE", 'ABC),
  ("OP__LOADK", 'ABx),
  ("OP__LOADBOOL", 'ABx)
            ])
