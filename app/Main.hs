module Main where

import Control.Applicative (optional)
import Options.Applicative (execParser, long, short, metavar, strOption, helper, fullDesc, progDesc, header, info, (<**>))
import System.IO (stdout, hPutStrLn, IOMode(..), withFile)

import Lib
import Format (showHeader, showFunction)

import qualified Lua
import Lua.Module (printModule)

main' :: IO ()
main' = do
  (file,out) <- execParser opts
  assembly <- case file of
    Just file' -> disassembleFile file'
    Nothing -> disassembleStdIn
  execute <- return $ case out of
    Just file' -> withFile file' WriteMode
    Nothing -> (\f -> f stdout)
  maybe (putStrLn "Failed to parse") (execute . writeAsm) assembly
    where
      writeAsm (Assembly header' fn) handle =
        do
          hPutStrLn handle (showHeader header')
          hPutStrLn handle (showFunction fn)
      argParser = (,) <$> inParser <*> outParser
      inParser = optional $ strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILENAME" )
      outParser = optional $ strOption
        ( long "out"
        <> short 'o'
        <> metavar "OUTPUT" )
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "Disassemble lua bytecode"
        <> header "delua - a lue bytecode disassembler" )

main :: IO ()
main = do
  (file,out) <- execParser opts
  assembly <- case file of
    Just file' -> Lua.disassembleFile file'
    Nothing -> Lua.disassembleStdIn
  execute <- return $ case out of
    Just file' -> withFile file' WriteMode
    Nothing -> (\f -> f stdout)
  maybe (putStrLn "Failed to parse") (execute . writeModule) assembly
    where
      writeModule mod' handle = hPutStrLn handle $ printModule mod'
      argParser = (,) <$> inParser <*> outParser
      inParser = optional $ strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILENAME" )
      outParser = optional $ strOption
        ( long "out"
        <> short 'o'
        <> metavar "OUTPUT" )
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "Disassemble lua bytecode"
        <> header "delua - a lue bytecode disassembler" )


