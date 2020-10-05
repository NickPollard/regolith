{-===========================================================================
  We assume that instructions are unsigned numbers.
  All instructions have an opcode in the first 6 bits.
  Instructions can have the following fields:
  'A' : 8 bits
  'B' : 9 bits
  'C' : 9 bits
  'Ax' : 26 bits ('A', 'B', and 'C' together)
  'Bx' : 18 bits ('B' and 'C' together)
  'sBx' : signed Bx

  A signed argument is represented in excess K; that is, the number
  value is the unsigned value minus K. K is exactly the maximum value
  for that argument (so that -max is represented by 0, and +max is
  represented by 2*max), which is half the maximum for the corresponding
  unsigned argument.
===========================================================================-}

module Opcode (
  OpCode(..),
  OpMode(..),
  OpArgs(..),
  parseOp,
  opModeFor
) where

import Data.Int (Int32)
import Data.Word (Word8)

--enum OpMode {iABC, iABx, iAsBx, iAx};  /* basic instruction format */

data OpMode = ABC  -- 8/9/9
            | ABx  -- 8/18
            | AsBx -- 8/signed 18
            | Ax   -- 26

data OpArgs = ArgsABC Int32 Int32 Int32
            | ArgsABx Int32 Int32
            | ArgsAsBx Int32 Int32
            | ArgsAx Int32

instance Show OpArgs where
  show (ArgsABC  a b c) = show a <> " " <> show b <> " " <> show c
  show (ArgsABx  a bx ) = show a <> " " <> show bx
  show (ArgsAsBx a sBx) = show a <> " " <> show sBx
  show (ArgsAx   ax   ) = show ax

data OpCode
  = OP_MOVE
  | OP_LOADK
  | OP_LOADKX
  | OP_LOADBOOL
  | OP_LOADNIL
  | OP_GETUPVAL
  | OP_GETTABUP
  | OP_GETTABLE
  | OP_SETTABUP
  | OP_SETUPVAL
  | OP_SETTABLE
  | OP_NEWTABLE
  | OP_SELF
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_MOD
  | OP_POW
  | OP_DIV
  | OP_IDIV
  | OP_BAND
  | OP_BOR
  | OP_BXOR
  | OP_SHL
  | OP_SHR
  | OP_UNM
  | OP_BNOT
  | OP_NOT
  | OP_LEN
  | OP_CONCAT
  | OP_JMP
  | OP_EQ
  | OP_LT
  | OP_LE
  | OP_TEST
  | OP_TESTSET
  | OP_CALL
  | OP_TAILCALL
  | OP_RETURN
  | OP_FORLOOP
  | OP_FORPREP
  | OP_TFORCALL
  | OP_TFORLOOP
  | OP_SETLIST
  | OP_CLOSURE
  | OP_VARARG
  | OP_EXTRAARG
  deriving (Show)

  {-
data OpCode = OP_MOVE
            | OP_LOADI
            | OP_LOADF
            | OP_LOADK
            | OP_LOADKX
            | OP_LOADBOOL
            | OP_LOADNIL
            | OP_GETUPVAL
            | OP_SETUPVAL
            | OP_GETTABUP
            | OP_GETTABLE
            | OP_GETI
            | OP_GETFIELD
            | OP_SETTABUP
            | OP_SETTABLE
            | OP_SETI
            | OP_SETFIELD
            | OP_NEWTABLE
            | OP_SELF
            | OP_ADDI
            | OP_SUBI
            | OP_MULI
            | OP_MODI
            | OP_POWI
            | OP_DIVI
            | OP_IDIVI
            | OP_ADDK
            | OP_SUBK
            | OP_MULK
            | OP_MODK
            | OP_POWK
            | OP_DIVK
            | OP_IDIVK
            | OP_BANDK
            | OP_BORK
            | OP_BXORK
            | OP_SHRI
            | OP_SHLI
            | OP_ADD
            | OP_SUB
            | OP_MUL
            | OP_MOD
            | OP_POW
            | OP_DIV
            | OP_IDIV
            | OP_BAND
            | OP_BOR
            | OP_BXOR
            | OP_SHL
            | OP_SHR
            | OP_UNM
            | OP_BNOT
            | OP_NOT
            | OP_LEN
            | OP_CONCAT
            | OP_CLOSE
            | OP_TBC
            | OP_JMP
            | OP_EQ
            | OP_LT
            | OP_LE
            | OP_EQK
            | OP_EQI
            | OP_LTI
            | OP_LEI
            | OP_GTI
            | OP_GEI
            | OP_TEST
            | OP_TESTSET
            | OP_CALL
            | OP_TAILCALL
            | OP_RETURN
            | OP_RETURN0
            | OP_RETURN1
            | OP_FORLOOP1
            | OP_FORPREP1
            | OP_FORLOOP
            | OP_FORPREP
            | OP_TFORPREP
            | OP_TFORCALL
            | OP_TFORLOOP
            | OP_SETLIST
            | OP_CLOSURE
            | OP_VARARG
            | OP_PREPVARARG
            | OP_EXTRAARG
            deriving (Show)
            -}

  {-
parseOp :: Word8 -> Maybe OpCode
parseOp op = case op of
  0 -> Just OP_MOVE
  1 -> Just OP_LOADI
  2 -> Just OP_LOADF
  3 ->  Just OP_LOADK
  4 ->  Just OP_LOADKX
  5 ->  Just OP_LOADBOOL
  6 ->  Just OP_LOADNIL
  7 ->  Just OP_GETUPVAL
  8 ->  Just OP_SETUPVAL
  9 ->  Just OP_GETTABUP
  10 ->  Just OP_GETTABLE
  11 ->  Just OP_GETI
  12 ->  Just OP_GETFIELD
  13 ->  Just OP_SETTABUP
  14 ->  Just OP_SETTABLE
  15 ->  Just OP_SETI
  16 ->  Just OP_SETFIELD
  17 ->  Just OP_NEWTABLE
  18 ->  Just OP_SELF
  19 ->  Just OP_ADDI
  20 ->  Just OP_SUBI
  21 ->  Just OP_MULI
  22 ->  Just OP_MODI
  23 ->  Just OP_POWI
  24 ->  Just OP_DIVI
  25 ->  Just OP_IDIVI
  26 ->  Just OP_ADDK
  27 ->  Just OP_SUBK
  28 ->  Just OP_MULK
  29 ->  Just OP_MODK
  30 ->  Just OP_POWK
  31 ->  Just OP_DIVK
  32 ->  Just OP_IDIVK
  33 ->  Just OP_BANDK
  34 ->  Just OP_BORK
  35 ->  Just OP_BXORK
  36 ->  Just OP_SHRI
  37 ->  Just OP_SHLI
  38 ->  Just OP_ADD
  39 ->  Just OP_SUB
  40 ->  Just OP_MUL
  41 ->  Just OP_MOD
  42 ->  Just OP_POW
  43 ->  Just OP_DIV
  44 ->  Just OP_IDIV
  45 ->  Just OP_BAND
  46 ->  Just OP_BOR
  47 ->  Just OP_BXOR
  48 ->  Just OP_SHL
  49 ->  Just OP_SHR
  50 ->  Just OP_UNM
  51 ->  Just OP_BNOT
  52 ->  Just OP_NOT
  53 ->  Just OP_LEN
  54 ->  Just OP_CONCAT
  55 ->  Just OP_CLOSE
  56 ->  Just OP_TBC
  57 ->  Just OP_JMP
  58 ->  Just OP_EQ
  59 ->  Just OP_LT
  60 ->  Just OP_LE
  61 ->  Just OP_EQK
  62 ->  Just OP_EQI
  63 ->  Just OP_LTI
  64 ->  Just OP_LEI
  65 ->  Just OP_GTI
  66 ->  Just OP_GEI
  67 ->  Just OP_TEST
  68 ->  Just OP_TESTSET
  69 ->  Just OP_CALL
  70 ->  Just OP_TAILCALL
  71 ->  Just OP_RETURN
  72 ->  Just OP_RETURN0
  73 ->  Just OP_RETURN1
  74 ->  Just OP_FORLOOP1
  75 ->  Just OP_FORPREP1
  76 ->  Just OP_FORLOOP
  77 ->  Just OP_FORPREP
  78 ->  Just OP_TFORPREP
  79 ->  Just OP_TFORCALL
  80 ->  Just OP_TFORLOOP
  81 ->  Just OP_SETLIST
  82 ->  Just OP_CLOSURE
  83 ->  Just OP_VARARG
  84 ->  Just OP_PREPVARARG
  85 ->  Just OP_EXTRAARG
  _ -> Nothing
-}

parseOp :: Word8 -> Maybe OpCode
parseOp op = case op of
  0 -> Just OP_MOVE
  1 -> Just OP_LOADK
  2 -> Just OP_LOADKX
  3 -> Just OP_LOADBOOL
  4 -> Just OP_LOADNIL
  5 -> Just OP_GETUPVAL
  6 -> Just OP_GETTABUP
  7 -> Just OP_GETTABLE
  8 -> Just OP_SETTABUP
  9 -> Just OP_SETUPVAL
  10 -> Just OP_SETTABLE
  11 -> Just OP_NEWTABLE
  12 -> Just OP_SELF
  13 -> Just OP_ADD
  14 -> Just OP_SUB
  15 -> Just OP_MUL
  16 -> Just OP_MOD
  17 -> Just OP_POW
  18 -> Just OP_DIV
  19 -> Just OP_IDIV
  20 -> Just OP_BAND
  21 -> Just OP_BOR
  22 -> Just OP_BXOR
  23 -> Just OP_SHL
  24 -> Just OP_SHR
  25 -> Just OP_UNM
  26 -> Just OP_BNOT
  27 -> Just OP_NOT
  28 -> Just OP_LEN
  29 -> Just OP_CONCAT
  30 -> Just OP_JMP
  31 -> Just OP_EQ
  32 -> Just OP_LT
  33 -> Just OP_LE
  34 -> Just OP_TEST
  35 -> Just OP_TESTSET
  36 -> Just OP_CALL
  37 -> Just OP_TAILCALL
  38 -> Just OP_RETURN
  39 -> Just OP_FORLOOP
  40 -> Just OP_FORPREP
  41 -> Just OP_TFORCALL
  42 -> Just OP_TFORLOOP
  43 -> Just OP_SETLIST
  44 -> Just OP_CLOSURE
  45 -> Just OP_VARARG
  46 -> Just OP_EXTRAARG
  _ -> Nothing


opModeFor :: OpCode -> OpMode
opModeFor OP_MOVE = ABC
opModeFor OP_LOADK = ABx
opModeFor OP_GETTABUP = ABC
opModeFor OP_SETTABUP = ABC
opModeFor OP_ADD = ABC
opModeFor OP_CALL = ABC
opModeFor OP_TAILCALL = ABC
opModeFor OP_RETURN = ABC
opModeFor OP_CLOSURE = ABx
opModeFor _ = ABC
