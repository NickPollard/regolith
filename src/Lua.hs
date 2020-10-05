module Lua where

import Data.Binary.Get (Get, runGetOrFail)
import Data.Either.Extra (eitherToMaybe)
import Data.Tuple.Extra (thd3)
import qualified Data.ByteString.Lazy as Lazy

import Lua.Module (readModule, SomeModule)

-- TODO(nickpollard) - MAIN TASK: get Lua.disassemble to work
disassemble :: Lazy.ByteString -> Maybe SomeModule
disassemble bytecode = getMaybe readModule bytecode

disassembleFile :: FilePath -> IO (Maybe SomeModule)
disassembleFile filename = do
  bs <- Lazy.readFile filename
  return $ disassemble bs

disassembleStdIn :: IO (Maybe SomeModule)
disassembleStdIn = do
  bs <- Lazy.getContents
  return $ disassemble bs

getMaybe :: Get a -> Lazy.ByteString -> Maybe a
getMaybe get input = fmap thd3 . eitherToMaybe $ runGetOrFail get input
