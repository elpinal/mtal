{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module MTAL
  ( ObjectFile(..)
  , Heap(..)
  , Types(..)
  , TypeDecl
  , Interface
  , KT(..)
  , wf
  , compatible
  , link
  , produce
  , executable
  , exec
  , Label(..)
  , File(..)
  , Inst(..)
  ) where

import qualified Data.Map.Lazy as Map
import Data.Monoid
import qualified Data.Set as Set

class Types h => Heap h where
  -- |
  -- Merges two heaps.
  -- Alpha-conversion of local labels is required to avoid name clashes.
  merge :: h -> h -> h

  typeOf :: Map.Map Label (Kind h) -> Map.Map Label (Type h) -> Map.Map Label (Type h) -> h -> Bool

  entryConform :: h -> Label -> Bool

  typeDecl :: h -> TypeDecl h
  interfaceOf :: h -> Interface h

  labels :: h -> Set.Set Label
  labels = Map.keysSet . interfaceOf

type TypeDecl h = Map.Map Label (Kind h, Type h)

data KT h
  = K (Kind h)
  | T (Type h)

getK :: Interface h -> Map.Map Label (Kind h)
getK = Map.mapMaybe f
  where
    f (K x) = Just x
    f _ = Nothing

getT :: Interface h -> Map.Map Label (Type h)
getT = Map.mapMaybe f
  where
    f (T x) = Just x
    f _ = Nothing

type Interface h = Map.Map Label (KT h)

class Types h where
  type Type h = t | t -> h
  type Kind h = k | k -> h

  equal :: Type h -> Type h -> Bool
  equalK :: Kind h -> Kind h -> Bool

  wfHeap :: Map.Map Label (Kind h) -> [Type h] -> Bool
  kindOf :: Map.Map Label (Kind h) -> Type h -> Kind h -> Bool

dom :: Heap h => h -> Set.Set Label
dom = labels

class File f where
  emptyFile :: f

class Inst i where
  jmp :: Label -> i

newtype Label = Label String
  deriving (Eq, Ord, Show)

data ObjectFile h = ObjectFile
  { heap :: h
  , imports :: Interface h
  , exports :: Interface h
  }

kinding :: Types h => Map.Map Label (Kind h) -> TypeDecl h -> Bool
kinding m = getAll . foldMap (\(k, t) -> All $ kindOf m t k) . Map.elems

isSubKindEnvOf :: Types h => Map.Map Label (Kind h) -> Map.Map Label (Kind h) -> Bool
isSubKindEnvOf = flip $ Map.isSubmapOfBy equalK

isSubinterfaceOf :: Heap h => Interface h -> Interface h -> Bool
isSubinterfaceOf i1 i2 = (getK i1 `isSubKindEnvOf` getK i2) && Map.isSubmapOfBy equal (getT i2) (getT i1)

-- | Determines well-formedness.
wf :: Heap h => ObjectFile h -> Bool
wf o = and
  [ interfaceOf (heap o) `isSubinterfaceOf` exports o
  , Map.keysSet (imports o) `Set.disjoint` dom (heap o)
  , kinding (getK (imports o) `Map.union` fmap fst (typeDecl $ heap o)) $ typeDecl $ heap o
  -- FIXME: missing a judgment.
  ]

compatibleInterface :: Types h => Interface h -> Interface h -> Bool
compatibleInterface i1 i2 = and
  [ getAll $ foldMap f $ Map.intersectionWith (,) (getK i1) (getK i2)
  , getAll $ foldMap g $ Map.intersectionWith (,) (getT i1) (getT i2)
  ]
  where
    f (k1, k2) = All $ equalK k1 k2
    g (t1, t2) = All $ equal t1 t2

-- Given two well formed object files, `compatible o1 o2` returns whether they
-- are link compatible or not.
compatible :: Heap h => ObjectFile h -> ObjectFile h -> Bool
compatible o1 o2 = and
  [ Map.keysSet (exports o1) `Set.disjoint` Map.keysSet (exports o2)
  , imports o1 `compatibleInterface` imports o2
  , imports o1 `compatibleInterface` exports o2
  , imports o2 `compatibleInterface` exports o1
  ]

-- Assumes link compatibility and well-formedness.
link :: Heap h => ObjectFile h -> ObjectFile h -> Maybe (ObjectFile h)
link o1 o2 =
  if dom (heap o1) `Set.disjoint` dom (heap o2)
    then return $ ObjectFile
      { heap = heap o1 `merge` heap o2
      , imports = imports o1 `Map.union` imports o2 Map.\\ exports o1 Map.\\ exports o2
      , exports = exports o1 `Map.union` exports o2 -- Note that link compatibility ensures these maps are disjoint.
      }
    else Nothing

executable :: Heap h => h -> Label -> Bool
executable h entry = and
  [ wfHeap (getK $ interfaceOf h) (Map.elems $ getT $ interfaceOf h)
  , kinding (fmap fst $ typeDecl h) (typeDecl h)
  , typeOf (fst <$> typeDecl h) (snd <$> typeDecl h) (getT $ interfaceOf h) h
  , entryConform h entry
  ]

-- Produces a well-formed executable (really?).
produce :: Heap h => ObjectFile h -> Label -> Maybe (h, Label)
produce o entry =
  if wf o && Map.null (imports o) && entryConform (heap o) entry && entry `Map.member` exports o
    then return (heap o, entry)
    else Nothing

exec :: (Heap h, File f, Inst i) => h -> Label -> Maybe (h, f, i)
exec h entry =
  if executable h entry
    then return (h, emptyFile, jmp entry)
    else Nothing
