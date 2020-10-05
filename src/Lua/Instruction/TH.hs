{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lua.Instruction.TH where

import Control.Monad (join)
import Language.Haskell.TH (Type(..), Q, mkName, Name, Con(..), Dec(..))

import Lua.Instruction.Args (ArgFmt)

data OpCode'
  = OP__MOVE
  | OP__LOADK
  | OP__LOADBOOL
  | OP__LOADNIL
  | OP__CALL
  | OP__RETURN

type family OpArgs' (o :: OpCode') :: ArgFmt

mkOpArgs :: String -> Name -> Q [Dec]
mkOpArgs name args =
  let name' = mkName name in [d| type instance OpArgs' $(return (PromotedT name')) = $(return (PromotedT args)) |]

mkOpArgss :: [(String, Name)] -> Q [Dec]
mkOpArgss items = join <$> traverse (uncurry mkOpArgs) items

-- Create the OpCode type declaration
-- mkEnum Foo [Bar, Baz] =>
--   data Foo
--     = Bar
--     | Baz
mkEnum :: String -> [String] -> Dec
mkEnum typename cons = DataD [] (mkName typename) [] Nothing cons' []
  where cons' = flip NormalC [] . mkName <$> cons

mkGadt :: String -> Type -> [(String, String)] -> Dec
mkGadt typename enumT cons =
  DataD [] (mkName typename) [] (Just kind) cons' []
    where cons' = mkCon <$> cons
          kind = AppT (AppT ArrowT enumT) StarT
          mkCon (name, opType) = GadtC [mkName name] [] (AppT (ConT $ mkName typename) (PromotedT (mkName opType)))
