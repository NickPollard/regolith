{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Control.Monad.Reader (ReaderT)
import Data.Binary (Get)
import Data.Int (Int32, Int64)
import Data.Word (Word8)

import Opcode (OpCode, OpArgs)

type Parser a = ReaderT Endianness Get a

-- Currently we only support Lua 5.3 and 5.1
data LuaVersion = Lua5_3
                | Lua5_1

data Endianness = BigEndian
                | LittleEndian

data TypeSize = Bytes4
              | Bytes8

data DataTypeSizes = DataTypeSizes { tInt :: TypeSize
                                   , tSizeT :: TypeSize
                                   , tInstruction :: TypeSize
                                   , tLuaInteger :: TypeSize
                                   , tLuaNumber :: TypeSize
                                   }

data Assembly = Assembly Header Function

-- Function Prototype
data Function = Function { _name :: String
                         , _beginDefined :: Int32
                         , _endDefined :: Int32
                         , _insts :: [Instruction]
                         , _consts :: [TValue]
                         , _upvalues :: [Upvalue]
                         , _protos :: [Function]
                         , _debug :: DebugInfo
                         }

data DebugInfo = DebugInfo { _lines :: [Int32]
                           , _locvars :: [LocVar]
                           , _upvalueNames :: [Maybe String]
                           } deriving Show

data LocVar = LocVar (Maybe String) Int32 Int32 deriving Show

data Upvalue = Upvalue Word8 Word8 deriving Show

data TValue = TNil
            | TBool Bool
            | TFloat LuaNumber
            | TInt LuaInteger
            | TStr (Maybe String)

instance Show TValue where
  show  TNil     = "nil"
  show (TBool b) = show b
  show (TFloat (LuaNumber n)) = show n
  show (TInt (LuaInteger i)) = show i
  show (TStr str) = maybe "<empty>" enquote str

enquote :: String -> String
enquote s = "\"" <> s <> "\""

newtype LuaInteger = LuaInteger { runInteger :: Int64 }
newtype LuaNumber = LuaNumber { runNumber :: Double }

-- Lua bytecode file Header
data Header = Header Version Format Size Size Size Size Size

-- Individual instruction with arguments
data Instruction = Instruction OpCode OpArgs

-- Datatype size (e.g. size of Int, Num, size_t)
type Size = Word8

-- Lua File Format (should be LUAC_FORMAT)
type Format = Word8

-- Lua Version absent patch (e.g. 5.3)
data Version = Version Major Minor
type Major = Word8
type Minor = Word8

instance Show Version where
  show (Version ma mi) = show ma <> "." <> show mi
