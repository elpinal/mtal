{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TAL
  (
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import qualified Data.Map.Lazy as Map

import qualified MTAL as M

data Inst
  = Mov Reg Operand
  | Add Reg Reg Operand
  deriving (Eq, Show)

newtype Reg = Reg Int
  deriving (Eq, Ord, Show)

data Operand
  = Register Reg
  | Value Val
  deriving (Eq, Show)

data Val
  = Const Int
  | Label M.Label
  deriving (Eq, Show)

data Block = Block
  { insts :: [Inst]
  , jmp :: Operand
  }
  deriving (Eq, Show)

newtype File = File (Map.Map Reg Val)
  deriving (Eq, Show)

newtype Heap = Heap (Map.Map M.Label Block)
  deriving (Eq, Show)

data Machine = Machine
  { heap :: Heap
  , file :: File
  , counter :: Block -- The program counter.
  }
  deriving (Eq, Show)

data Type
  = Int
  | Code Context
  deriving (Eq, Show)

newtype Context = Context { getContext :: Map.Map Reg Type }
  deriving (Eq, Show)

newtype HeapContext = HeapContext (Map.Map M.Label Type)
  deriving (Eq, Show)

data TypeError
  = UnboundRegister Reg
  | UnboundLabel M.Label
  | NotInt Type
  | NotCode Type
  | HeapDomainMismatch Heap HeapContext
  | TypeMismatch Type Type
  | ContextMismatch Context Context

type E = '[State Context, Reader HeapContext, Error TypeError]

-- TODO: better name.
type HeapLevel = '[Reader HeapContext, Error TypeError]

typeOfRegister :: Members '[State Context, Error TypeError] r => Reg -> Eff r Type
typeOfRegister r = do
  Context m <- get
  maybe (throwError $ UnboundRegister r) return $ Map.lookup r m

typeOfValue :: Members HeapLevel r => Val -> Eff r Type
typeOfValue (Const _) = return Int
typeOfValue (Label l) = do
  HeapContext m <- ask
  maybe (throwError $ UnboundLabel l) return $ Map.lookup l m

typeOfOperand :: Members E r => Operand -> Eff r Type
typeOfOperand (Register r) = typeOfRegister r
typeOfOperand (Value v) = typeOfValue v

typeOfInst :: Members E r => Inst -> Eff r ()
typeOfInst (Mov r o) = typeOfOperand o >>= write r
typeOfInst (Add d s o) = do
  typeOfRegister s >>= requireInt
  typeOfOperand o >>= requireInt
  write d Int

write :: Member (State Context) r => Reg -> Type -> Eff r ()
write r t = modify $ Context . Map.insert r t . getContext

requireInt :: Member (Error TypeError) r => Type -> Eff r ()
requireInt Int = return ()
requireInt t = throwError $ NotInt t

jmpTo :: Members '[State Context, Error TypeError] r => Type -> Eff r ()
jmpTo (Code ctx0) = get >>= \ctx -> unless (ctx0 == ctx) $ throwError $ ContextMismatch ctx0 ctx
jmpTo t = throwError $ NotCode t

typeOfBlock :: Members HeapLevel r => Context -> Block -> Eff r ()
typeOfBlock ctx b = evalState ctx $ mapM_ typeOfInst (insts b) >> typeOfOperand (jmp b) >>= jmpTo

typeOfFile :: Members HeapLevel r => File -> Eff r Context
typeOfFile (File f) = Context <$> traverse typeOfValue f

typeOfHeap :: Members HeapLevel r => Heap -> Eff r ()
typeOfHeap (Heap h) = do
  HeapContext hctx <- ask
  if Map.keysSet h /= Map.keysSet hctx
    then throwError $ HeapDomainMismatch (Heap h) $ HeapContext hctx
    else sequence_ $ Map.intersectionWith (\t b -> fromCode t >>= (`typeOfBlock` b)) hctx h

fromCode :: Member (Error TypeError) r => Type -> Eff r Context
fromCode (Code ctx) = return ctx
fromCode t = throwError $ NotCode t

wfMachine :: Members HeapLevel r => Machine -> Eff r ()
wfMachine m = do
  typeOfHeap $ heap m
  ctx <- typeOfFile $ file m
  typeOfBlock ctx $ counter m
