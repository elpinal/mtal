{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TAL
  ( Typing(..)

  , Machine(..)
  , Heap(..)
  , File(..)

  , Block(..)
  , Reg(..)
  , Val(..)
  , Inst(..)
  , Operand(..)

  , Type(..)
  , HeapContext(..)
  , Context(..)

  -- * Errors
  , TypeError(..)
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
  | ContextMismatch Context Context

type E = '[State Context, Reader HeapContext, Error TypeError]

-- TODO: better name.
type HeapLevel = '[Reader HeapContext, Error TypeError]

class Typing a where
  type TypingEffs a :: [* -> *]
  type Output a

  typeOf :: Members (TypingEffs a) r => a -> Eff r (Output a)

instance Typing Reg where
  type TypingEffs Reg = '[State Context, Error TypeError]
  type Output Reg = Type

  typeOf r = do
    Context m <- get
    maybe (throwError $ UnboundRegister r) return $ Map.lookup r m

instance Typing Val where
  type TypingEffs Val = HeapLevel
  type Output Val = Type

  typeOf (Const _) = return Int
  typeOf (Label l) = do
    HeapContext m <- ask
    maybe (throwError $ UnboundLabel l) return $ Map.lookup l m

instance Typing Operand where
  type TypingEffs Operand = E
  type Output Operand = Type

  typeOf (Register r) = typeOf r
  typeOf (Value v) = typeOf v

instance Typing Inst where
  type TypingEffs Inst = E
  type Output Inst = ()

  typeOf (Mov r o) = typeOf o >>= write r
  typeOf (Add d s o) = do
    typeOf s >>= requireInt
    typeOf o >>= requireInt
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
typeOfBlock ctx b = evalState ctx $ mapM_ typeOf (insts b) >> typeOf (jmp b) >>= jmpTo

instance Typing File where
  type TypingEffs File = HeapLevel
  type Output File = Context

  typeOf (File f) = Context <$> traverse typeOf f

instance Typing Heap where
  type TypingEffs Heap = HeapLevel
  type Output Heap = ()

  typeOf (Heap h) = do
    HeapContext hctx <- ask
    if Map.keysSet h /= Map.keysSet hctx
      then throwError $ HeapDomainMismatch (Heap h) $ HeapContext hctx
      else sequence_ $ Map.intersectionWith (\t b -> fromCode t >>= (`typeOfBlock` b)) hctx h

fromCode :: Member (Error TypeError) r => Type -> Eff r Context
fromCode (Code ctx) = return ctx
fromCode t = throwError $ NotCode t

instance Typing Machine where
  type TypingEffs Machine = HeapLevel
  type Output Machine = ()

  typeOf m = do
    typeOf $ heap m
    ctx <- typeOf $ file m
    typeOfBlock ctx $ counter m
