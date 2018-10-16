module MTAL
  (
  ) where

import qualified Data.Set as Set

class Heap h where
  labels :: h -> Set.Set Label
  usedLabels :: h -> Set.Set Label
  -- | Merges two disjoint heaps.
  merge :: h -> h -> h

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
  , imports :: Set.Set Label
  , exports :: Set.Set Label
  }

-- Determines well-formedness.
wf :: Heap h => ObjectFile h -> Bool
wf o =
  let domain = dom $ heap o in and
  [ exports o `Set.isSubsetOf` domain
  , imports o `Set.disjoint` domain
  , usedLabels (heap o) `Set.isSubsetOf` (domain `Set.union` imports o)
  ]

-- Given two well formed object files, `compatible o1 o2` returns whether they
-- are link compatible or not.
compatible :: Heap h => ObjectFile h -> ObjectFile h -> Bool
compatible o1 o2 = exports o1 `Set.disjoint` exports o2

link :: Heap h => ObjectFile h -> ObjectFile h -> Maybe (ObjectFile h)
link o1 o2 =
  if dom (heap o1) `Set.disjoint` dom (heap o2)
    then return $ ObjectFile
      { heap = heap o1 `merge` heap o2
      , imports = imports o1 `Set.union` imports o2 Set.\\ exports o1 Set.\\ exports o2
      , exports = exports o1 `Set.union` exports o2
      }
    else Nothing

executable :: Heap h => h -> Label -> Bool
executable h entry = entry `Set.member` dom h && usedLabels h `Set.isSubsetOf` dom h

produce :: Heap h => ObjectFile h -> Label -> Maybe (h, Label)
produce o entry =
  if wf o && Set.null (imports o) && entry `Set.member` exports o
    then return (heap o, entry)
    else Nothing

exec :: (Heap h, File f, Inst i) => h -> Label -> Maybe (h, f, i)
exec h entry =
  if executable h entry
    then return (h, emptyFile, jmp entry)
    else Nothing
