{-# LANGUAGE FlexibleContexts #-}

module Format (
  showHeader,
  showFunction,
) where

import Prelude hiding (readFile, lines)
import qualified Prelude
import Control.Monad.Reader (MonadReader, runReader, ask)
import qualified Data.ByteString.Lazy as Lazy
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Word (Word8)

import Opcode (OpArgs(..), OpCode(..))
import Parse (parseHeader, parseBody)
import Types (Assembly(..), Header(..), Instruction(..), Function(..), Upvalue(..), TValue(..), LuaInteger(..), LuaNumber(..), DebugInfo(..))

-- TODO: use `Text` instead of `String`

showHeader :: Header -> String
showHeader (Header v f s1 s2 s3 s4 s5) =
  ".LUA_HEADER\n; VERSION " <> show v <>
  "\n; FORMAT " <> show f <>
  "\n; SIZES:" <>
  "\n;   int " <> show s1 <>
  "\n;   size_t " <> show s2 <>
  "\n;   Instruction " <> show s3 <>
  "\n;   lua_Integer " <> show s4 <>
  "\n;   lua_Number " <> show s5

lines :: [String] -> String
lines [] = ""
lines xs = '\n' : intercalate "\n" xs

indent :: Int -> String -> String
indent n = unlines . map  (replicate n ' ' <>) . Prelude.lines

nest :: String -> String
nest = indent 2

showFunction :: Function -> String
showFunction (Function src begin end insts consts upvs protos debug) =
     "; "<> src <> ":" <> show begin <> "-" <> show end
  <> "\n.instructions"
  <> (lines $ showInst (_upvalueNames debug) consts <$> insts)
  <> "\n.consts" <> (lines $ showConst <$> consts)
  <> "\n.upvalues" <> (lines $ showUpvalue (_upvalueNames debug) <$> upvs)
  <> "\n.protos" <> (nest . lines $ showFunction <$> protos)
  <> "\n.debug" <> show debug

showUpvalue :: [Maybe String] -> Upvalue -> String
showUpvalue names (Upvalue instack index) = "; UPVALUE " <> show instack <> " " <> show index <> ": " <> upvalueName names index
  --maybe "<unnamed>" (\s -> "\"" <> s <> "\"" (names !! fromIntegral index)

upvalueName :: (Integral n) => [Maybe String] -> n -> String
upvalueName names index = case (names !! fromIntegral index) of
                            --Just s -> "\"" <> s <> "\""
                            Just s -> s
                            Nothing -> "<unnamed>"

showConst :: TValue -> String
showConst t = "; CONST " <> show t

showInst :: [Maybe String] -> [TValue] -> Instruction -> String
showInst upvalues consts inst@(Instruction op args) =
  pad asm width <> comment
    where asm = show op <> " " <> show args
          comment = runReader (commentInst upvalues inst) consts
          width = 30
          pad msg w = msg <> (replicate (w - length msg) ' ')

commentInst :: MonadReader [TValue] m => [Maybe String] -> Instruction -> m String
commentInst _ (Instruction OP_MOVE (ArgsABC a b _)) = return $ "; R(" <> show a <> ") := R(" <> show b <> ")"
commentInst _ (Instruction OP_GETTABUP (ArgsABC a b c)) = (\c' -> "; R(" <> show a <> ") := UpValue["<> show b <>"][" <> c' <> "]") <$> rk c
--commentInst ups (Instruction OP_SETTABUP (ArgsABC a b c)) = (\b' c' -> "; UpValue["<> show a <>"][" <> b' <> "] := " <> c') <$> rk b <*> rk c
commentInst ups (Instruction OP_SETTABUP (ArgsABC a b c)) = (\b' c' -> "; " <> upvalueName ups a <> "[" <> b' <> "] := " <> c') <$> rk b <*> rk c
commentInst _ (Instruction OP_SETTABLE (ArgsABC a b c)) = (\b' c' -> "; R("<> show a <>")[" <> b' <> "] := " <> c') <$> rk b <*> rk c
commentInst _ (Instruction OP_CLOSURE (ArgsABx a bx)) = return $ "; R(" <> show a <>") := closure(KPROTO[" <> show bx <> "])"
commentInst _ (Instruction OP_RETURN (ArgsABC a b _)) = return $ "; return R(" <> show a <>"), ... , R(" <> (if (b == 0) then "top" else show (a + b - 2)) <> ")" -- TODO
commentInst _ (Instruction OP_ADD (ArgsABC a b c)) = (\b' c' -> "; R(" <> show a <>") = " <> b' <> " + " <> c') <$> rk b <*> rk c
commentInst _ (Instruction OP_TAILCALL (ArgsABC a b _)) = return $ "; return R(" <> show a <>")(R(" <> show (a + 1) <> "), ... ,R(" <> show (a + b - 1) <> "))"
commentInst _ _ = return ""

rk :: MonadReader [TValue] m => Int32 -> m String
rk n = do
  let kconst = 256
  consts <- ask
  return $ case n of
            n' | n' >= kconst -> show $ consts !! fromIntegral (n' - kconst)
            otherwise         -> "R(" <> show n <> ")"
