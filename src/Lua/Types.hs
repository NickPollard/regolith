module Lua.Types where

import Data.Binary (get)
import Data.Binary.Get (Get, getWord8, getInt64le)
import Data.Binary.Put (Put, putWord8)
import Data.Int (Int64)
import Data.Word (Word8)

-- Lua native 'Integer' type
newtype LuaInteger = LuaInteger { runInteger :: Int64 }
-- Lua native 'Number' type, which includes floating point
newtype LuaNumber = LuaNumber { runNumber :: Double }

loadInteger :: Get LuaInteger
loadInteger = LuaInteger <$> getInt64le -- TODO(nickpollard) - handle endianness

loadNumber :: Get LuaNumber
loadNumber = LuaNumber <$> get

-- Type of dynamic Lua values, indeed by type
-- Stored constants are of this type
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

-- TODO(nickpollard) - what are these two fields?
data Upvalue = Upvalue Word8 Word8 deriving Show

-- Size in bytes, used to describe datatype sizes (e.g. size of LuaInt, LuaNum, size_t)
newtype Size = Size { runSize :: Word8 } deriving Show

readSize :: Get Size
readSize = Size <$> getWord8

writeSize :: Size -> Put
writeSize = putWord8 . runSize

-- Lua File Format (should be LUAC_FORMAT)
type Format = Word8

