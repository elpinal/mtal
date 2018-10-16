{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module MTAL
  (
  ) where

import Data.Monoid
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

class Types h => Heap h where
  type Type h = t | t -> h
  labels :: h -> Set.Set Label
  usedLabels :: h -> Set.Set Label
  -- | Merges two disjoint heaps.
  merge :: h -> h -> h

  assumption :: h -> Map.Map Label (Type h)
  typeOf :: Map.Map Label (Type h) -> h -> Bool

  entryConform :: h -> Label -> Bool

class Types h where
  equal :: Type h -> Type h -> Bool
  isSubinterfaceOf :: Map.Map Label (Type h) -> Map.Map Label (Type h) -> Bool
  wfInterface :: Map.Map Label (Type h) -> Bool

dom :: Heap h => h -> Set.Set Label
dom = labels

class File f where
  emptyFile :: f

class Inst i where
  jmp :: Label -> i

newtype Label = Label String
  deriving (Eq, Ord)

data ObjectFile h = ObjectFile
  { heap :: h
  , imports :: Map.Map Label (Type h)
  , exports :: Map.Map Label (Type h)
  }

-- Determines well-formedness.
wf :: Heap h => ObjectFile h -> Bool
wf o = and
  [ wfInterface $ imports o
  , assumption (heap o) `isSubinterfaceOf` exports o
  , Map.keysSet (imports o) `Set.disjoint` dom (heap o)
  , typeOf (imports o `Map.union` assumption (heap o)) (heap o)
  ]

compatibleInterface :: Types h => Map.Map Label (Type h) -> Map.Map Label (Type h) -> Bool
compatibleInterface i1 i2 = getAll $ foldMap (All . uncurry equal) $ Map.intersectionWith (,) i1 i2

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
executable h entry = wfInterface (assumption h) && typeOf (assumption h) h && entryConform h entry

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
