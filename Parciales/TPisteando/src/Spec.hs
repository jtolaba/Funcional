{-# OPTIONS_GHC -Wno-missing-fields #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec

-- MODELOS INICIALES --
ferrari = Auto "Ferrari" "F50" (0,0) 65 0 ["La nave", "El fierro", "Ferrucho"]
lamborghini = Auto "Lamborghini" "Diablo" (4,7) 73 0 ["Lambo", "La bestia"]
fiat = Auto "Fiat" "600" (27,33) 44 0 ["La Bocha", "La bolita", "Fitito"]
peugeot = Auto "Peugeot" "504" (0,0) 40 0 ["El rey del desierto"]

correrTests :: IO ()
correrTests = hspec $ do

-- 2.A funcionalidad estaEnBuenEstado
  describe "Funcionalidad: estaEnBuenEstado" $ do
      it "Peugeot no esta en buen estado" $ do
        peugeot `shouldSatisfy` not . estaEnBuenEstado
      it "Lamborghini tiempo en pista 99 seg y desgaste chasis 7 está en buen estado" $ do
        let lamborghini = Auto {marca = "Lamborghini", tiempoDeCarrera= 99, desgaste = (7,7) }
        lamborghini `shouldSatisfy` estaEnBuenEstado
      it "Fiat tiempo en pista 99 seg y desgaste chasis 33 no está en buen estado" $ do
        let fiat = Auto {marca = "Fiat", tiempoDeCarrera= 99, desgaste = (13,33) }
        fiat `shouldSatisfy` not . estaEnBuenEstado
      it "Ferrari tiempo en pista 130 seg, desgaste ruedas 50 y chasis 30 esta en buen estado" $ do
        let ferrari = Auto {marca = "Ferrari", tiempoDeCarrera= 130, desgaste = (50,30) }
        ferrari `shouldSatisfy` estaEnBuenEstado
      it "Ferrari tiempo en pista 15 seg, desgaste ruedas 50 y chasis 45 no esta en buen estado" $ do
        let ferrari = Auto {marca = "Ferrari", tiempoDeCarrera= 10, desgaste = (50,45) }
        ferrari `shouldSatisfy` not . estaEnBuenEstado
      it "Ferrari tiempo en pista 150 seg, desgaste ruedas 70 y chasis 30 no esta en buen estado" $ do
        let ferrari = Auto {marca = "Ferrari", tiempoDeCarrera= 150, desgaste = (70,30) }
        ferrari `shouldSatisfy` not.estaEnBuenEstado

-- 2.B funcionalidad noDaMas
  describe "Funcionalidad: noDaMas" $ do
      it "Ferrari con desgaste de ruedas 20 y chasis 90 no da para mas" $ do
        let ferrari = Auto {marca = "Ferrari", desgaste = (20,90), apodo = ["La Ferrari"]}
        ferrari `shouldSatisfy` noDaMas
      it "Ferrari con desgaste de ruedas 90 y chasis 20 da para mas" $ do
        let ferrari = Auto {marca = "Ferrari", desgaste = (90,20), apodo = ["La Ferrari"]}
        ferrari `shouldSatisfy` not . noDaMas
      it "Lamborghini con desgaste de ruedas 90 chasis 20 no da para más" $ do
        let lamborghini = Auto {marca = "Lamborghini", desgaste = (90,20), apodo= ["bestia"] }
        lamborghini `shouldSatisfy` noDaMas
      it "Lamborghini da para más" $ do
        lamborghini `shouldSatisfy` not . noDaMas

-- 2.C Funcionalidad: esUnChiche
  describe "Funcionalidad: esUnChiche" $ do
      it "Lamborghini es un chiche" $ do
        lamborghini `shouldSatisfy` esUnChiche
      it "Lamborghini con desgaste de ruedas 90 y chasis 20 no es un chiche" $ do
        let lamborghini = Auto "Lamborghini" "Diablo" (90, 20) 73 0 ["Lambo", "La bestia"]
        lamborghini `shouldSatisfy` not . esUnChiche
      it "Ferrari con desgaste de ruedas 20 y chasis 90 no es un chiche" $ do
        let ferrari = Auto "Ferrari" "modelo F50" (20, 90) 65 0 ["La nave", "El fierro", "Ferrucho"]
        ferrari `shouldSatisfy`  not . esUnChiche
      it "Ferrari es un chiche" $ do
        ferrari `shouldSatisfy` esUnChiche

-- 2.D Funcionalidad: esUnaJoya
  describe "Funcionalidad: esUnaJoya" $ do
    it "Peugeot Es una joya (tiene cero desgaste y tiene un apodo) " $ do
        peugeot `shouldSatisfy` esUnaJoya
    it "Ferrari No es una Joya (no tiene desgaste pero tiene más de un apodo.)" $ do
        ferrari `shouldSatisfy` not . esUnaJoya

-- 2.E Funcionalidad: nivelChetez
  describe "Funcionalidad: nivelChetez" $ do
    it "Nivel de chetez auto de marca Ferrari es 180" $ do
      nivelChetez ferrari `shouldBe` 180

-- 2.F Funcionalidad: capacidadSupercalifragilisticaespialidosa
  describe "Funcionalidad: capacidadSupercalifragilisticaespialidosa" $ do
    it "Para un Ferrari, tengo la cant de letras del primer apodo (La nave) que es 7" $ do
      capacidadSupercalifragilisticaespialidosa ferrari `shouldBe` 7

-- 2.G Funcionalidad: esRiesgosoAuto
  describe "Funcionalidad: esUnAutoriesgoso" $ do
      it "Un auto de marca Lamborghini Valor de riesgo 29.2" $ do
        valorDeRiesgoAuto lamborghini `shouldBe` 29.2
      it "Un auto de marca Fiat Valor de riesgo 237.6 " $ do
        valorDeRiesgoAuto fiat `shouldBe` 237.6

-- 3.A Funcionalidad: repararUnAuto
  describe "Funcionalidad: repararUnAuto" $ do
      it "Reparar un Auto Fiat reduce desgaste de ruedas en 0 y chasis en 4.95" $ do
        desgaste (repararUnAuto fiat) `shouldBe` (0, 4.95)
      it "Reparar un Auto Ferrari reduce desgaste de ruedas en 0 y chasis en 0" $ do      
        desgaste (repararUnAuto ferrari) `shouldBe` (0, 0)

-- 3.B Funcionalidad: aplicarPenalidad
  describe "Funcionalidad: aplicarPenalidad" $ do
    let ferrariModificado = Auto {marca = "Ferrari", tiempoDeCarrera= 10, desgaste = (50,45) }
    it "Aplicar penalidad de 20 segundos a ferrari con 10 seg en pista lo deja con 30 seg" $ do
      tiempoDeCarrera (aplicarPenalidad ferrariModificado 20) `shouldBe` 30
    it "Aplicar penalidad de 0 segundos a ferrari con 10 seg en pista lo deja con 10 seg" $ do
      tiempoDeCarrera (aplicarPenalidad ferrariModificado 0) `shouldBe` 10

-- 3.C Funcionalidad: ponerNitro 
  describe "Funcionalidad: ponerNitro" $ do
    it "Poner nitro a un Fiat aumenta velocidad maxima a 52.8" $ do
      velocidadMaxima (ponerNitro fiat)  `shouldBe` 52.8
    it "Poner nitro a un Fiat que tiene 0 de velocidad maxima mantiene su velocidad maxima" $ do
      let fiatModificado = Auto {velocidadMaxima = 0 }
      velocidadMaxima (ponerNitro fiatModificado )  `shouldBe` 0

-- 3.D Funcionalidad: bautizarAuto
  describe "Funcionalidad: bautizarAuto" $ do
    it "Bautizar El diablo a un auto Lamborghini contiene en sus apodos El diablo" $ do
      let autoBautizado = bautizarAuto lamborghini "El diablo"
      apodo autoBautizado `shouldSatisfy` elem "El diablo"
    it "Bautizar El diablo a un auto Lamborghini sin apodos sólo tiene el apodo El diablo" $ do
      let lamborghiniModificado = Auto {apodo = []}
      apodo (bautizarAuto lamborghiniModificado "El diablo") `shouldBe` ["El diablo"] 

-- 3.E Funcionalidad: llevarAlDesermadero
  describe "Funcionalidad: llevarAlDesermadero" $ do
    let autoNuevo = llevarAlDesarmadero fiat "Tesla" "X"
    describe "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca Tesla modelo X" $ do
      it "La marca es Tesla" $ do
        marca autoNuevo `shouldBe` "Tesla"
      it "El modelo es X" $ do
        modelo autoNuevo `shouldBe` "X"
      it "Sólo tiene el apodo Nunca Taxi" $ do
        apodo autoNuevo `shouldBe` ["Nunca Taxi"]

-- 4.A Funcionalidad: atravesarCurva
  describe "Funcionalidad: atravesarCurva" $ do
    it "Ferrari al transitar curvaPeligrosa, tiene un desgaste ruedas de 15" $ do
      desgasteRuedas (transitarTramo curvaPeligrosa ferrari)  `shouldBe` 15
    it "Ferari al transitar curvaPeligrosa, tiene un desgaste chasis de 0" $ do
      desgasteChasis (transitarTramo curvaPeligrosa ferrari)  `shouldBe` 0
    it "Peugeot al transitar curvaPeligrosa, tiempo en pista 15" $ do
      tiempoDeCarrera (transitarTramo curvaPeligrosa peugeot)  `shouldBe` 15
    it "Ferrari al transitar curvaTranca, tiene un desgaste ruedas de 15" $ do
      desgasteRuedas (transitarTramo curvaTranca ferrari)  `shouldBe` 15
    it "Ferari al transitar curvaTranca, tiene un desgaste chasis de 0" $ do
      desgasteChasis (transitarTramo curvaTranca ferrari)  `shouldBe` 0
    it "Peugeot al transitar curvaTranca, tiempo en pista 27.5" $ do
      tiempoDeCarrera (transitarTramo curvaTranca peugeot)  `shouldBe` 27.5

-- 4.B Funcionalidad: atravesarRecta
  describe "Funcionalidad: AtravesarRecta" $ do
    it "Ferrari al transitar tramoRectoClassic, Tiene un desgaste de chasis de 7.15" $ do
      desgasteChasis (transitarTramo tramoRectroClassic ferrari)  `shouldBe` 7.15
    it "Ferrari al transitar tramoRectoClassic, tiempo en pista 11" $ do
      tiempoDeCarrera (transitarTramo tramoRectroClassic ferrari)  `shouldBe` 11
    it "Ferrari al transitar tramito, Tiene un desgaste de chasis de 2.6" $ do
      desgasteChasis (transitarTramo tramito ferrari)  `shouldBe` 2.6
    it "Ferrari al transitar tramito, tiempo en pista 4" $ do
      tiempoDeCarrera (transitarTramo tramito ferrari)  `shouldBe` 4

-- 4.C Funcionalidad: atravesarZigZag
  describe "Funcionalidad: atravesarZigZag" $ do
    it "Ferrari al transitar zigZagLoco, Tiene un desgaste de chasis de 5" $ do
      desgasteChasis (transitarTramo zigZagLoco ferrari)  `shouldBe` 5
    it "Ferrari al transitar zigZagLoco, Tiene un desgaste de ruedas de 32.5" $ do
      desgasteRuedas (transitarTramo zigZagLoco ferrari)  `shouldBe` 32.5
    it "Ferrari al transitar zigZagLoco, tiempo en pista 15" $ do
      tiempoDeCarrera (transitarTramo zigZagLoco ferrari)  `shouldBe` 15
    it "Ferrari al transitar casiCurva, Tiene un desgaste de chasis de 5" $ do
      desgasteChasis (transitarTramo casiCurva ferrari)  `shouldBe` 5
    it "Ferrari al transitar casiCurva, Tiene un desgaste de ruedas de 32.5" $ do
      desgasteRuedas (transitarTramo casiCurva ferrari)  `shouldBe` 6.5
    it "Ferrari al transitar casiCurva, tiempo en pista 3" $ do
      tiempoDeCarrera (transitarTramo casiCurva ferrari)  `shouldBe` 3

-- 4.D Funcionalidad: atravesarRulo
  describe "Funcionalidad: atravesarRulo" $ do
    it "Ferrari al transitar ruloClasico, Tiene un desgaste de chasis de 0" $ do
      desgasteChasis (transitarTramo ruloClasico ferrari)  `shouldBe` 0
    it "Ferrari al transitar ruloClasico, Tiene un desgaste de ruedas de 19.5" $ do
      desgasteRuedas (transitarTramo ruloClasico ferrari)  `shouldBe` 19.5
    it "Ferrari al transitar ruloClasico, tiempo en pista 1" $ do
      tiempoDeCarrera (transitarTramo ruloClasico ferrari)  `shouldBe` 1
    it "Ferrari al transitar deseoDeMuerte, Tiene un desgaste de chasis de 0" $ do
      desgasteChasis (transitarTramo deseoDeMuerte ferrari)  `shouldBe` 0
    it "Ferrari al transitar deseoDeMuerte, Tiene un desgaste de ruedas de 39" $ do
      desgasteRuedas (transitarTramo deseoDeMuerte ferrari)  `shouldBe` 39
    it "Ferrari al transitar deseoDeMuerte, tiempo en pista 2" $ do
      tiempoDeCarrera (transitarTramo deseoDeMuerte ferrari)  `shouldBe` 2

-- 5.A Funcionalidad: nivelDeJoyez 
  describe "Funcionalidad: nivelDeJoyez " $ do
    let peugeotModificado = Auto "Peugeot" "504" (0, 0) 65 50 ["“El rey del desierto"]
    let peugeotModificado2 = Auto "Peugeot" "504" (0, 0) 65 49 ["“El rey del desierto"]
    let listaDeAutos = [ferrari ,peugeotModificado, peugeotModificado2] --
    it "El nivelDeJoyes del grupo [ferrari, peugeot, peugeot2] es de 3" $ do
      nivelDeJoyez esUnaJoya listaDeAutos `shouldBe` 3.0

-- 5.B Funcionalidad: sonParaEntendidos
  describe "Funcionalidad: sonParaEntendidos" $ do
       
      let ferrari201 = Auto "Ferrari" "F50" (0, 0) 65 201 ["La nave", "El fierro", "Ferrucho"]
          ferrari200 = Auto "Ferrari" "F50" (0, 0) 65 200 ["La nave", "El fierro", "Ferrucho"]
          peugeotMalo = Auto "Peugeot" "504" (0, 0) 40 0 ["El rey del desierto"]
          lamborghini200 = Auto "Lamborghini" "Diablo" (4, 7) 73 200 ["Lambo", "La bestia"]

      it "El grupo no es para entendidos si el Ferrari supera el tiempo de 200" $ do
        let autos = [ferrari201, ferrari200]
        autos `shouldSatisfy` not . sonParaEntendidos

      it "El grupo no es para entendidos si el Peugeot no está en buen estado" $ do
        let autos = [ferrari200, peugeotMalo]
        autos  `shouldSatisfy` not . sonParaEntendidos

      it "El grupo es para entendidos si ambos autos están en buen estado y tienen tiempos <= 200" $ do
        let autos = [ferrari200, lamborghini200]
        autos  `shouldSatisfy` sonParaEntendidos