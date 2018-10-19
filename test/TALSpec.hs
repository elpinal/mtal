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

      wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.K ()
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, TLabel $ M.Label "i"), (Reg 1, TLabel $ M.Label "i")]
          , typeDeclH = mempty
          }
        }

      wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.T $ Code $ Context mempty
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, Int), (Reg 1, Int)]
          , typeDeclH = mempty
          }
        }

      wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.T Int -- A controversial case.
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.fromList [(Reg 0, Int), (Reg 1, Int)]
          , typeDeclH = mempty
          }
        }

      wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.T $ Code $ Context $ Map.fromList [(Reg 0, Int), (Reg 1, Int)]
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "i"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.singleton (Reg 1) Int
          , typeDeclH = mempty
          }
        }

      M.wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.T $ Code $ Context $ Map.fromList [(Reg 0, Int), (Reg 1, Int), (Reg 3, Int)]
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [Mov (Reg 0) $ Register $ Reg 1] $ Value $ Label $ M.Label "i"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.singleton (Reg 1) Int
          , typeDeclH = mempty
          }
        } `shouldBe` False

      wf M.ObjectFile
        { M.imports = mempty
        , M.exports = Map.singleton (M.Label "a") $ M.T $ Code $ Context $ mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ mempty
          , typeDeclH = mempty
          }
        }

      M.wf M.ObjectFile
        { M.imports = mempty
        , M.exports = Map.singleton (M.Label "a") $ M.T $ Code $ Context $ Map.singleton (Reg 20) Int
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [] $ Value $ Label $ M.Label "a"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ mempty
          , typeDeclH = mempty
          }
        } `shouldBe` False

      wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.T $ Code $ Context $ Map.singleton (Reg 0) Int
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [] $ Value $ Label $ M.Label "i"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.singleton (Reg 0) Int
          , typeDeclH = Map.singleton (M.Label "t") ((), Int)
          }
        }

      M.wf M.ObjectFile
        { M.imports = Map.singleton (M.Label "i") $ M.T $ Code $ Context $ Map.singleton (Reg 0) Int
        , M.exports = mempty
        , M.heap = H
          { heapH = Heap $ Map.singleton (M.Label "a") $ Block [] $ Value $ Label $ M.Label "i"
          , heapContextH = HeapContext $ Map.singleton (M.Label "a") $ Code $ Context $ Map.singleton (Reg 0) $ TLabel $ M.Label "t"
          , typeDeclH = Map.singleton (M.Label "t") ((), Int)
          }
        } `shouldBe` False
