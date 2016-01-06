module Dynamic.StructuralSpec where
import Dynamic.Structural
import Test.Hspec

spec :: Spec
        spec = 
        describe "interpret" $ do
          it "should not step past final state" $
            interpret (ENum 10) `shouldBe` ENum 10
