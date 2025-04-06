module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Test Ejercicio 11 funcion pesoPino" $ do
    it "pesoPino 1" $ do
      pesoPino 1 `shouldBe` 3
      
    it "pesoPino 301" $ do
      pesoPino 301 `shouldBe` 902
    
    it "pesoPino -1" $ do
      pesoPino (-1) `shouldBe` 0
    
    