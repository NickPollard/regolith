{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lua.Module where

import Data.Binary.Get (runGet, Get, getWord32le, getByteString)
import Data.Binary.Put (Put)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))

import Parse.Primitive (readByte)
import Lua.Function (Function(..), writeFunction, readFunction, printFunction)
import Lua.Types (Format, Size, readSize)
import Lua.Version (Version(..), V(..), Versioned(..), readVersion, compareV, LuaVersion(..))

-- | A Module containing Lua ByteCode of some dynamic version
type SomeModule = Versioned Module

-- | A Header for a Lua Module of some dynamic version
type SomeHeader = Versioned Header

-- | A Module containing Lua ByteCode of version `v`
data Module (v :: Version) = Module { header :: Header v
                                    , function :: Function v
                                    }

-- | A Header for some Lua version `v`
data Header (v :: Version) = Header { format :: Format
                                    , _sizeInt :: Size
                                    , _sizeSizeT :: Size
                                    , _sizeInst :: Size
                                    , _sizeLuaInt :: Size
                                    , _sizeLuaNum :: Size
                                    }

writeHeader :: Header v -> Put
writeHeader = error "writeHeader NYI" -- TODO

printHeader :: forall v. LuaVersion v => Header v -> String
printHeader (Header f s1 s2 s3 s4 s5) =
  ".LUA_HEADER\n; VERSION " <> show (luaVersion (Proxy @v)) <>
  "\n; FORMAT " <> show f <>
  "\n; SIZES:" <>
  "\n;   int " <> show s1 <>
  "\n;   size_t " <> show s2 <>
  "\n;   Instruction " <> show s3 <>
  "\n;   lua_Integer " <> show s4 <>
  "\n;   lua_Number " <> show s5

printModule :: SomeModule -> String
printModule (Versioned version (Module header' fn)) =
  case version of
    V51 -> printHeader header' <> "\n" <> printFunction version fn
    V53 -> printHeader header' <> "\n" <> printFunction version fn

-- Attempt to read a header of some unknown LUA version
readHeader :: Get SomeHeader
readHeader = do
  -- Lua files start with this literal
  readLuaLiteral
  -- Then Version encoded as 4/4 bits
  version <- readVersion
  -- Then a byte indicating format (should be "LUAC")
  fmt <- readFormat
  -- Then a magic constant is expected
  validateLuaData
  -- Then we list the sizes (in bytes) of 5 main data types
  size_int <- readSize
  size_size_t <- readSize
  size_inst <- readSize
  size_lua_int <- readSize
  size_lua_num <- readSize
  checkTestInt
  checkTestFloat
  return $ case version of
    Lua51 -> Versioned V51 $ Header fmt size_int size_size_t size_inst size_lua_int size_lua_num
    Lua53 -> Versioned V53 $ Header fmt size_int size_size_t size_inst size_lua_int size_lua_num
    where readLuaLiteral :: Get ()
          readLuaLiteral = do
            lua <- getWord32le
            if (lua == lit) then return () else fail "Missing LUA literal"
              -- TODO get the hex value of this string
              where lit = runGet getWord32le "\x1bLua"
          checkTestInt :: Get ()
          checkTestInt = return ()
          checkTestFloat :: Get ()
          checkTestFloat = return ()

-- Lua files are marked with a magic constant that is checked to ensure correct encoding
validateLuaData :: Get ()
validateLuaData = do
  magic_number <- getByteString 6
  if (magic_number == luac_data) then return () else fail "Missing valid LUAC_DATA marker"
    where luac_data :: ByteString
          luac_data = "\x19\x93\r\n\x1a\n"

readFormat :: Get Format
readFormat = readByte

-- Attempt to read a specific expected version
readHeaderV :: V v -> Get (Header v)
readHeaderV v = do
  Versioned headerV headr <- readHeader
  case compareV v headerV of
    Just Refl -> return headr
    Nothing -> fail "invalid type"

writeModule :: Module v -> Put
writeModule (Module hdr fn) = do
  writeHeader hdr
  writeFunction fn

readModule :: Get SomeModule
readModule = do
  hdr <- readHeader
  case hdr of
    Versioned V51 h -> Versioned V51 . Module h <$> readFunction V51 "<no parent>"
    Versioned V53 h -> Versioned V53 . Module h <$> readFunction V53 "<no parent>"

-- Attempt to read a specific expected version
readModuleV :: V v -> Get (Module v)
readModuleV version = do
  hdr <- readHeaderV version
  fn <- readFunction version "<no parent>"
  return $ Module hdr fn
