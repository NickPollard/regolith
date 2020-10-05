-- | How do we handle endianness?

-- Endian-aware eget
data GetE

class IsEndian m where
  endianness :: m Endianness

-- aka MonadReader Endianness

runGetE :: Endianness -> GetE a -> Get a
