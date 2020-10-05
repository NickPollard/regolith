{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lua.Function where

import Data.Binary.Get (Get, getWord32be)
import Data.Binary.Put (Put)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import Lua.Instruction (readInstruction, printInst, VInstruction)
import Lua.Types (TValue(..), Upvalue(..), loadNumber, loadInteger)
import Lua.Version (Version(..), V(..))

import Parse.Primitive (readByte, readString, readVectorN, readBool)

data DebugInfo = DebugInfo

-- A Lua Function Prototype
data Function (v :: Version) = Function { _name :: String
                                        , _defined :: (Word32, Word32)
                                        , _insts :: [VInstruction v]
                                        , _consts :: [TValue]
                                        , _upvalues :: [Upvalue]
                                        , _protos :: [Function v]
                                        , _debug :: DebugInfo
                                        }

writeFunction :: Function v -> Put
writeFunction = error "writeFunction NYI"
  {-
     do
     putString name
     putInt (fst _defined)
     putInt (snd _defined)
     for_ _insts writeInstruction
     for_ _consts writeConst
     for_ _upvalues writeUpvalue
     for_ _protos putFunction
     putDebug _debug
     -}

printFunction :: V v -> Function v -> String
printFunction v (Function name (begin, end) insts consts _ _ _) =
     "; "<> name <> ":" <> show begin <> "-" <> show end
  <> "\n.instructions"
  <> (unlines $ printInst v [] consts <$> insts) -- TODO parse and provide upvalue names from debug
    {-
  <> "\n.consts" <> (lines $ showConst <$> consts)
  <> "\n.upvalues" <> (lines $ showUpvalue (_upvalueNames debug) <$> upvs)
  <> "\n.protos" <> (nest . lines $ showFunction <$> protos)
  <> "\n.debug" <> show debug
  -}

--lines :: [String] -> String
--lines [] = ""
--lines xs = '\n' : intercalate "\n" xs

-- TODO(nickpollard) - implement readFunction
readFunction :: V v -> String -> Get (Function v)
readFunction v parent = do
  -- The source function is this function if named, or the parent if this fn is anon
  source <- fromMaybe parent <$> readString
  -- Debug info
  defined <- ((,) <$> getWord32be <*> getWord32be)
  _numparams <- readByte
  _is_vararg <- readByte
  _max_stack <- readByte
  instructions <- readInstructions v
  debugInfo <- readDebugInfo
  consts <- readConstants
  upvalues <- readUpvalues
  protos <- readProtos v source
  return $ Function source defined instructions consts upvalues protos debugInfo

-- | Read a top-level closure
readClosure :: V v -> Get (Function v)
readClosure version = do
  -- numupvalues
  _ <- readByte
  -- now read the function
  readFunction version "<no source>"

readInstructions :: V v -> Get [VInstruction v]
readInstructions = readVectorN . readInstruction

readConstants :: Get [TValue]
readConstants = readVectorN loadTValue

-- TODO(nickpollard) - move to types
loadTValue :: Get TValue
loadTValue = do
  type' <- readByte
  case type' of
    0 -> return TNil
    1 -> TBool <$> readBool
    3 -> TFloat <$> loadNumber
    19 -> TInt <$> loadInteger
    4 -> TStr <$> readString -- ShortStr
    20 -> TStr <$> readString -- LongStr
    e -> fail $ "Invalid TValue type: " <> show e

readUpvalues :: Get [Upvalue]
readUpvalues = readVectorN readUpvalue

readUpvalue :: Get Upvalue
readUpvalue = Upvalue <$> readByte <*> readByte

readProtos :: V v -> String -> Get [Function v]
readProtos version = readVectorN . readFunction version

-- TODO(nickpollard) - parse DebugInfo (and define DebugInfo struct)
readDebugInfo :: Get DebugInfo
readDebugInfo = return DebugInfo
{-
do
  lines' <- readVectorN readWord32be
  locvars <- readVectorN loadLocVar
  upvalueNames <- readVectorN readString
  return $ DebugInfo lines' locvars upvalueNames
    where loadLocVar = LocVar <$> readString <*> readInt LittleEndian <*> readInt LittleEndian
    -}
