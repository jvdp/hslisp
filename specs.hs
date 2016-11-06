import Test.Hspec

import Lisp

main :: IO ()
main = hspec $ do
  describe "evaluate" $ do
    it "should return a 1 when given a 1" $ do
      evaluate (LAtom (LInt 1)) [] `shouldBe` Right (LAtom (LInt 1))

    it "should return a 2 when given a 2" $ do
      evaluate (LAtom (LInt 2)) [] `shouldBe` Right (LAtom (LInt 2))

    it "should return a 3.14 when given a 3.14" $ do
      evaluate (LAtom (LFlo 3.14)) [] `shouldBe` Right (LAtom (LFlo 3.14))

    it "should return a true when given a true" $ do
      evaluate (LAtom LTrue) [] `shouldBe` Right (LAtom LTrue)

    it "should return a false when given a false" $ do
      evaluate (LAtom LFalse) [] `shouldBe` Right (LAtom LFalse)

    it "should return a nil when given a nil" $ do
      evaluate (LAtom LNil) [] `shouldBe` Right (LAtom LNil)

    it "should return a string when given a string" $ do
      evaluate (LAtom (LStr "foo")) [] `shouldBe` Right (LAtom (LStr "foo"))

    it "should return a 1 when given a foo" $ do
      evaluate (LAtom (LSym "foo")) [("foo", LAtom (LInt 1))] `shouldBe` Right (LAtom (LInt 1))

    it "should return a 2 when given a bar" $ do
      evaluate (LAtom (LSym "bar")) [("bar", LAtom (LInt 2))] `shouldBe` Right (LAtom (LInt 2))

    it "should crash when given a baz" $ do
      evaluate (LAtom (LSym "baz")) [] `shouldBe` Left "cannot find baz"

    it "should return a cons when given a cons" $ do
      evaluate (LCons (LAtom (LInt 1)) (LAtom (LInt 2))) [] `shouldBe` Right (LCons (LAtom (LInt 1)) (LAtom (LInt 2)))

    it "should return a nested cons when given a nested cons" $ do
      evaluate (LCons (LAtom (LInt 1)) (LCons (LAtom (LInt 2)) (LAtom (LInt 3)))) [] `shouldBe` Right (LCons (LAtom (LInt 1)) (LCons (LAtom (LInt 2)) (LAtom (LInt 3))))

    it "should return a 3 when given a (+ 1 2)" $ do
      evaluate (mkLList [LAtom (LSym "+"), LAtom (LInt 1), LAtom (LInt 2)]) [("+", LFunc plus)] `shouldBe` Right (LAtom (LInt 3))

    it "should return a 3 when given a (+ 1 (+ 2 3))" $ do
      evaluate (mkLList [LAtom (LSym "+"), LAtom (LInt 1), LAtom (LInt 2)]) [("+", LFunc plus)] `shouldBe` Right (LAtom (LInt 6))

    -- it "should return a 6 when given a (+ 1 (+ 2 3))" $ do
    --   evaluate (mkLList [LAtom (LSym "+"), LAtom (LInt 1), (mkLList [LAtom (LSym "+"), LAtom (LInt 2), LAtom (LInt 3)])]) [("+", LFunc plus)] `shouldBe` Right (LAtom (LInt 6))

    -- it "should return a lambda when given a lambda" $ do
    --   evaluate (LLambda ["x"] (LAtom (LSym "x"))) `shouldBe` (Right (LLambda ["x"] (LAtom (LSym "x"))))

  describe "#isList" $ do
    it "should return false when not given a list - (1 . 2)" $ do
      isList (LCons (LAtom (LInt 1)) (LAtom (LInt 2))) `shouldBe` False

    it "should return false when not given a list" $ do
      isList (LFunc plus) `shouldBe` False

    it "should return true when given a list - (1)" $ do
      isList (LCons (LAtom (LInt 1)) (LAtom LNil)) `shouldBe` True


plus :: [LExpr] -> LExpr
plus [LAtom (LInt x), LAtom (LInt y)] = LAtom (LInt (x + y))




