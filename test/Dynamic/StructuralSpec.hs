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
