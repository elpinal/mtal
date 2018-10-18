module TALSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import TAL
import qualified MTAL as M

wf :: (HasCallStack, M.Heap h) => M.ObjectFile h -> Expectation
wf o = M.wf o `shouldBe` True

spec :: Spec
spec = do
  describe "wf" $
    it "checks well-formedness" $ do
      wf M.ObjectFile
        { M.imports = mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap mempty
          , heapContextH = HeapContext mempty
          , typeDeclH = mempty
          }
        }

      wf M.ObjectFile
        { M.imports = mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ mempty
          , typeDeclH = mempty
          }
        }

      wf M.ObjectFile
        { M.imports = mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, Int), (Reg 1, Int)]
          , typeDeclH = mempty
          }
        }

      wf M.ObjectFile
        { M.imports = mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, Int), (Reg 1, Int)]
          , typeDeclH = Map.singleton (M.Label "t") ((), Int)
          }
        }

      wf M.ObjectFile
        { M.imports = mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, TLabel $ M.Label "t"), (Reg 1, TLabel $ M.Label "t")]
          , typeDeclH = Map.singleton (M.Label "t") ((), Int)
          }
        }

      M.wf M.ObjectFile
        { M.imports = mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, TLabel $ M.Label "none"), (Reg 1, TLabel $ M.Label "none")]
          , typeDeclH = mempty
          }
        } `shouldBe` False
