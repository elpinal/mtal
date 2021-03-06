{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

  , TypeBinding(..)

  , H(..)

  -- * Errors
  , TypeError(..)
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import qualified Data.Map.Lazy as Map
import Data.Monoid
import qualified Data.Set as Set

import qualified MTAL as M

-- | An instruction.
data Inst
  = Mov Reg Operand
  | Add Reg Reg Operand
  deriving (Eq, Show)

-- | A register.
newtype Reg = Reg Int
  deriving (Eq, Ord, Show)

data Operand
  = Register Reg
  | Value Val
  deriving (Eq, Show)

-- | A value.
data Val
  = Const Int
  | Label M.Label
  | Roll M.Label Val
  | Unroll Val
  deriving (Eq, Show)

-- | An instruction sequence.
data Block = Block
  { insts :: [Inst]
  , jmp :: Operand
  }
  deriving (Eq, Show)

-- | A register file.
newtype File = File (Map.Map Reg Val)
  deriving (Eq, Show)

newtype Heap = Heap (Map.Map M.Label Block)
  deriving (Eq, Show, Semigroup)

data Machine = Machine
  { heap :: Heap
  , file :: File
  , counter :: Block -- ^ The program counter.
  }
  deriving (Eq, Show)

data H = H
  { heapH :: Heap
  , heapContextH :: HeapContext
  , typeDeclH :: Map.Map M.Label ((), Type)
  }
  deriving (Eq, Show)

data Type
  = Int
  | Code Context
  | TLabel M.Label
  deriving (Eq, Show)

newtype Context = Context { getContext :: Map.Map Reg Type }
  deriving (Eq, Show)

newtype HeapContext = HeapContext { getHeapContext :: Map.Map M.Label Type }
  deriving (Eq, Show, Semigroup)

newtype TypeBinding = TypeBinding (Map.Map M.Label Type)
  deriving (Eq, Show)

newtype KindEnv = KindEnv (Set.Set M.Label)
  deriving (Eq, Show)

data TypeError
  = UnboundRegister Reg
  | UnboundLabel M.Label
  | NotInt Type
  | NotCode Type
  | NotLabel Type
  | MissingHeapTypeInformation Heap HeapContext
  | ContextMismatch Context Context
  | NoSuchLabelType M.Label
  | CouldNotRoll Type M.Label
  deriving (Eq, Show)

type E = '[State Context, Reader HeapContext, Error TypeError]

-- TODO: better name.
type HeapLevel = '[Reader HeapContext, Error TypeError]

runHeapLevel :: HeapContext -> Eff (Reader HeapContext ': Error TypeError ': r) a -> Eff r (Either TypeError a)
runHeapLevel r e = runError $ runReader r e

class Typing a where
  type TypingEffs a :: [* -> *]
  type Output a

  typeOf :: Members (Reader KindEnv ':Reader TypeBinding ': TypingEffs a) r => a -> Eff r (Output a)

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
  typeOf (Roll l v) = do
    t1 <- typeOf v
    TypeBinding m <- ask
    t2 <- maybe (throwError $ NoSuchLabelType l) return $ Map.lookup l m
    if t1 == t2
      then return $ TLabel l
      else throwError $ CouldNotRoll t1 l
  typeOf (Unroll v) = do
    t <- typeOf v
    TypeBinding m <- ask
    l <- fromLabel t
    maybe (throwError $ NoSuchLabelType l) return $ Map.lookup l m

fromLabel :: Member (Error TypeError) r => Type -> Eff r M.Label
fromLabel (TLabel l) = return l
fromLabel t = throwError $ NotLabel t

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

typeOfBlock :: Members (Reader KindEnv ': Reader TypeBinding ': HeapLevel) r => Context -> Block -> Eff r ()
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
    if Map.keysSet h `Set.isSubsetOf` Map.keysSet hctx
      then sequence_ $ Map.intersectionWith (\t b -> fromCode t >>= (`typeOfBlock` b)) hctx h
      else throwError $ MissingHeapTypeInformation (Heap h) $ HeapContext hctx

-- Returns a well-formed context.
fromCode :: Members '[Reader KindEnv, Error TypeError] r => Type -> Eff r Context
fromCode (Code ctx) = wf ctx >> return ctx
fromCode t = throwError $ NotCode t

instance Typing Machine where
  type TypingEffs Machine = HeapLevel
  type Output Machine = ()

  typeOf m = do
    typeOf $ heap m
    ctx <- typeOf $ file m
    typeOfBlock ctx $ counter m

instance M.Types H where
  type Type H = Type
  type Kind H = ()

  equalK () () = True
  equal = (==)

  wfHeap km = getAll . foldMap (\t -> All $ M.kindOf km t ())

  kindOf km t () =
    case run $ runError $ runReader (KindEnv $ Map.keysSet km) $ wf t of
      Right () -> True
      Left e -> let _ = e :: TypeError in False

instance M.Heap H where
  merge h1 h2 = H
    { heapH = heapH h1 <> heapH h2
    , heapContextH = heapContextH h1 <> heapContextH h2
    , typeDeclH = typeDeclH h1 <> typeDeclH h2
    }

  entryConform h l =
    case Map.lookup l $ getHeapContext $ heapContextH h of
      Just (Code (Context m)) -> Map.null m
      Just _ -> False
      Nothing -> False

  typeDecl = typeDeclH

  interfaceOf h =
    let kenv = M.K . fst <$> typeDeclH h in
    let tenv = fmap M.T . getHeapContext $ heapContextH h in
    Map.union kenv tenv -- Notice: assume these are distinct.

  typeOf kenv tbind tenv h =
    case run $ runReader (KindEnv $ Map.keysSet kenv) $ runReader (TypeBinding tbind) $ runHeapLevel (HeapContext tenv) $ typeOf (heapH h) of
      Right () -> True
      Left _ -> False

instance M.File File where
  emptyFile = File mempty

instance M.Inst Block where
  jmp l = Block [] $ Value $ Label l

class WellFormed a where
  wf :: Members '[Reader KindEnv, Error TypeError] r => a -> Eff r ()

instance WellFormed Context where
  wf (Context m) = mapM_ wf m

instance WellFormed Type where
  wf Int = return ()
  wf (Code ctx) = wf ctx
  wf (TLabel l) = do
    KindEnv ls <- ask
    unless (l `Set.member` ls) $
      throwError $ NoSuchLabelType l
