module Lua.Write where

writeModule :: SomeModule -> m ByteString
writeModule = runPut putModule

putModule :: SomeModule -> Put
putModule (Module51 mod) = putModuleV mod
putModule (Module53 mod) = putModuleV mod

putModuleV :: LuaVersion v => Module v -> Put
putModuleV (Module header function) = put header >> put function

putHeader :: Header v -> Put
putHeader version (Header format sizeInt sizeSizeT sizeInst sizeLuaInt sizeLuaNum) = do
  writeLuaLiteral
  putVersion (luaVersion (proxy @v))
  putByte format
  putLuacData
  putSize sizeInt
  putSize sizeSizeT
  putSize sizeInst
  putSize sizeLuaInt
  putSize sizeLuaNum
  putTestInt
  putTestFloat

putFunction :: Function v -> Put
putFunction = return ()

putByte :: Byte -> Put
putByte = putWord8

putSize :: Size -> Put
putSize = putWord8

putVersion :: VersionRepr -> Put
putVersion (Version major minor) = runBitPut $ do
  Bits.putWord8 4 major
  Bits.putWord8 4 minor

putFunction :: Header -> Put
putHeader (Function ) = undefined

-- Special markers
putLuaLiteral :: Put
putLuaLiteral = do
  lua <- putBytestring "\x1bLua"
  -- TODO - calculate the actual numeric value (can just read this from a file?)
  
putTestInt :: Put
putTestInt = do
  -- This is for 64 bit; do we support 32 bit?
  putInt64 0x5678

putTestFloat :: Put
putTestFoat = return ()

putLuacData :: Get ()
putLuacData = putByteString luac_data

luac_data :: ByteString
luac_data = "\x19\x93\r\n\x1a\n"
