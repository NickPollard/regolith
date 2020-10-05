{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
    disassembleFile,
    disassembleStdIn,
    Assembly(..),
    ) where

import Prelude hiding (readFile)
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as Lazy

import Parse (parseHeader, parseBody)
import Types (Assembly(..))

disassembleFile :: FilePath -> IO (Maybe Assembly)
disassembleFile filename = do
  bs <- Lazy.readFile filename
  return $ disassemble bs

disassembleStdIn :: IO (Maybe Assembly)
disassembleStdIn = do
  bs <- Lazy.getContents
  return $ disassemble bs

disassemble :: Lazy.ByteString -> Maybe Assembly
disassemble bytecode = Just $ runGet (Assembly <$> parseHeader <*> parseBody) bytecode
