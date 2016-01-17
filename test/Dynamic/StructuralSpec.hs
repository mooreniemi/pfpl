module Dynamic.StructuralSpec where
import Static.Checker
import Dynamic.Structural
import Test.Hspec

spec :: Spec
spec = 
  describe "interpret" $ do
    describe "should not step past final state" $ do
      it "should recognize num as final value" $ do
        interpret (ENum 10) `shouldBe` ENum 10
      it "should recognize str as final value" $ do
         interpret (EStr "value") `shouldBe` EStr "value"
      it "should reduce addition AST" $ do
         interpret (EAdd (ENum 10) (ENum 10)) `shouldBe` ENum 20
      it "should reduce concatenation AST" $ do
         interpret (ECat (EStr "me") (EStr "ow")) `shouldBe` EStr "meow"
    describe "should evaluate any subexpressions" $ do
      it "should do transition on sub1 first" $ do
         interpret (EAdd (EAdd (ENum 1) (ENum 1)) (ENum 2)) `shouldBe` ENum 4
      it "or sub2 first" $ do
         interpret (EAdd (ENum 2) (EAdd (ENum 1) (ENum 1))) `shouldBe` ENum 4
      it "should do transition on sub1 str first" $ do
         interpret (ECat (ECat (EStr "hel") (EStr "lo")) (EStr " world")) `shouldBe` EStr "hello world"
    describe "defs should become transparent over values" $ do
       it "a wrapped string is just a string" $ do
         interpret (EDef (EStr "string") "x" (EId "x")) `shouldBe` EStr "string"
       it "a wrapped int is just an int" $ do
          interpret (EDef (ENum 10) "y" (EId "y")) `shouldBe` ENum 10
       it "allow for higher expressions, like addition operation" $ do
          interpret (EDef (ENum 1) "one" (EAdd (ENum 1) (EId "one"))) `shouldBe` ENum 2
       it "allow for higher expressions, or multiplication operation" $ do
          interpret (EDef (ENum 3) "three" (EMult (ENum 1) (EId "three"))) `shouldBe` ENum 3
