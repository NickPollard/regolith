{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lua.Version where

import Data.Binary.Get (Get)
import Data.Binary.Bits.Get (runBitGet)
import qualified Data.Binary.Bits.Get as Bits
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))
import Data.Word (Word8)

-- | Coproduct of supported Lua types. The type and constructors are promoted to the kind and type
--   level respectively.
--
--   Currently we support only the following Lua versions:
--
--     5.1
--     5.3
data Version = Lua51
             | Lua53

-- | GADT whose constructors witness the `Version` type parameter, allowing us to retrieve it in
--   pattern matches
data V :: Version -> * where
  V51 :: V 'Lua51
  V53 :: V 'Lua53

newtype Major = Major Word8
newtype Minor = Minor Word8

-- | A dynamic numeric representation of the Lua version absent patch (e.g. 5.3)
--   We use this when we need to serialize or deserialize a version
data DynVersion = DynVersion Major Minor

instance Show DynVersion where
  show (DynVersion (Major ma) (Minor mi)) = show ma <> "." <> show mi

readDynVersion :: Get DynVersion
readDynVersion = runBitGet $ do
  major <- Major <$> Bits.getWord8 4
  minor <- Minor <$> Bits.getWord8 4
  return $ DynVersion major minor

readVersion :: Get Version
readVersion = do
  dyn <- readDynVersion
  case dyn of
    DynVersion (Major 5) (Minor 3) -> return Lua53
    DynVersion (Major 5) (Minor 1) -> return Lua51
    _ -> fail "Unsupported lua version parsed"

-- | Map each typelevel `Version` to a numeric
class LuaVersion (v :: Version) where
  luaVersion :: Proxy v -> DynVersion

instance LuaVersion 'Lua53 where
  luaVersion _ = DynVersion (Major 5) (Minor 3)
instance LuaVersion 'Lua51 where
  luaVersion _ = DynVersion (Major 5) (Minor 1)

repr :: Version -> DynVersion
repr Lua53 = luaVersion (Proxy @'Lua53)
repr Lua51 = luaVersion (Proxy @'Lua51)

-- | Tag a version-indexed type with a GADT witness whilst erasing the type. The GADT witness allows
--   us to later recall the version when pattern matching
data Versioned (f :: Version -> *) where
  Versioned :: V v -> f v -> Versioned f

compareV :: V a -> V b -> Maybe (a :~: b)
compareV V51 V51 = Just Refl
compareV V53 V53 = Just Refl
compareV _ _ = Nothing
