{-# LANGUAGE OverloadedStrings #-}

module Parse where

--import Control.Monad (replicateM)
import Data.Binary (Get, get, put)
import Data.Binary.Get (runGet, getWord32le, getWord8, getByteString, getWord64le, getInt64le)
import Data.Binary.Put (runPut)
import Data.Binary.Bits.Get (runBitGet)
import qualified Data.Binary.Bits.Get as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Word (Word32)
import System.IO.Unsafe (unsafePerformIO)

import Opcode (parseOp, OpMode(..), OpArgs(..), opModeFor)
import Parse.Primitive (readInt, getInt32be, readVectorN, readString, readByte, readBool)
import Types (Endianness(..), Size, Version(..), Header(..), Instruction(..), Function(..), DebugInfo(..), Upvalue(..), TValue(..), LocVar(..), LuaInteger(..), LuaNumber(..))

loadVersion :: Get Version
loadVersion = runBitGet $ do
  major <- Bits.getWord8 4
  minor <- Bits.getWord8 4
  return $ Version major minor

parseHeader :: Get Header
parseHeader = do
  luaLiteral
  version <- loadVersion
  format <- readByte
  _ <- checkLuacData
  -- TODO: use these sizes
  size_int <- loadSize
  size_size_t <- loadSize
  size_inst <- loadSize
  size_lua_int <- loadSize
  size_lua_num <- loadSize
  loadTestInt
  loadTestFloat
  return $ Header version format size_int size_size_t size_inst size_lua_int size_lua_num

loadTestInt :: Get ()
loadTestInt = do
  (LuaInteger n) <- loadInteger
  if (n == 0x5678) then return () else fail "Invalid test int; data corrupted"

loadInteger :: Get LuaInteger
loadInteger = LuaInteger <$> getInt64le

loadNumber :: Get LuaNumber
loadNumber = LuaNumber <$> get

loadTestFloat :: Get ()
loadTestFloat = do
  _ <- getWord64le
  -- TODO
    {-
  d <- getByteString 8
  d' <- return $ runGet (get :: Get Double) (Lazy.fromStrict $ Strict.reverse d)
  d'' <- return $ unsafePerformIO (print d' >> return d')
  if (d'' == 370.5) then return () else fail "Invalid test floating-point num; data corrupted"
  -}
  return ()

loadSize :: Get Size
loadSize = getWord8

checkLuacData :: Get ()
checkLuacData = do
  luac <- getByteString 6
  if (luac == luac_data) then return () else fail "Missing valid LUAC_DATA marker"

luac_data :: ByteString
luac_data = "\x19\x93\r\n\x1a\n"

luaLiteral :: Get ()
luaLiteral = do
  lua <- getWord32le
  if (lua == lit) then return () else fail "Missing LUA literal"
      where lit = runGet getWord32le "\x1bLua"

parseBody :: Get Function
parseBody = loadClosure

debugp :: Show a => String -> a -> a
debugp label a = unsafePerformIO $ putStr label >> print a >> return a

loadClosure :: Get Function
loadClosure = do
  _ <- debugp "numupvalues " <$> getWord8
  loadFunction "<no source>"

loadFunction :: String -> Get Function
loadFunction parent = do
  source <- maybe parent id <$> readString
  lineDefined <- readInt LittleEndian
  lastLineDefined <- readInt LittleEndian
  _numparams <- readByte
  _is_vararg <- readByte
  _max_stack <- readByte
  insts <- loadCode
  Function source lineDefined lastLineDefined insts
    <$> loadConstants
    <*> loadUpvalues
    <*> loadProtos source
    <*> loadDebug

loadConstants :: Get [TValue]
loadConstants = readVectorN loadTValue

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

loadUpvalues :: Get [Upvalue]
loadUpvalues = readVectorN loadUpvalue

loadUpvalue :: Get Upvalue
loadUpvalue = Upvalue <$> readByte <*> readByte

loadProtos :: String -> Get [Function]
loadProtos = readVectorN . loadFunction

loadDebug :: Get DebugInfo
loadDebug = do
  lines' <- readVectorN $ readInt LittleEndian
  locvars <- readVectorN loadLocVar
  upvalueNames <- readVectorN readString
  return $ DebugInfo lines' locvars upvalueNames
    where loadLocVar = LocVar <$> readString <*> readInt LittleEndian <*> readInt LittleEndian

loadCode :: Get [Instruction]
loadCode = readVectorN $ parseInstruction LittleEndian

parseInstruction :: Endianness -> Get Instruction
parseInstruction BigEndian = parseInstructionBe
parseInstruction LittleEndian = runLe parseInstructionBe
  where
    runLe f =
      do
        le <- Lazy.fromStrict <$> getByteString 4
        let be = Lazy.reverse le
        return $ runGet f be

-- Expects BigEndian
parseInstructionBe :: Get Instruction
parseInstructionBe = runBitGet $ do
  argBits <- Bits.getWord32be 26
  opcode <- Bits.getWord8 6
  op <- maybe (fail "not a valid opcode") return (parseOp opcode)
  let args = parseOpArgs' (opModeFor op) argBits
  return $ Instruction op args

parseOpArgs' :: OpMode -> Word32 -> OpArgs
parseOpArgs' op bits = runGet (runBitGet $ parseOpArgs op) (runPut (put bits))

parseOpArgs :: OpMode -> Bits.BitGet OpArgs
parseOpArgs ABC  = do
  _ <- Bits.getWord8 6
  -- For some reason, Lua orders these `a, c, b`
  b <- getInt32be 9
  c <- getInt32be 9
  a <- getInt32be 8
  return $ ArgsABC a b c
parseOpArgs ABx  = do
  _ <- Bits.getWord8 6
  b <- getInt32be 18
  a <- getInt32be 8
  return $ ArgsABx a b
parseOpArgs AsBx  = do
  _ <- Bits.getWord8 6
  b <- getInt32be 18
  a <- getInt32be 8
  return $ ArgsAsBx a b
parseOpArgs Ax   = do
  _ <- Bits.getWord8 6
  ArgsAx   <$> getInt32be 26
