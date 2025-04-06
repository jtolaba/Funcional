module Library where

import Data.Ratio (numerator)
import PdePreludat
import Test.Hspec (xcontext)
import Control.Exception.Base (nonTermination)

doble :: Number -> Number
doble = (* 2)

--- Ejercicio 1 ---
esMultiploDeTres :: Number -> Bool
esMultiploDeTres dividendo = mod dividendo 3 == 0

--- Ejercicio 2 ---
esMultiploDe :: Number -> Number -> Bool
esMultiploDe dividendo divisor = mod dividendo divisor == 0

--- Ejercicio 3 ---
cubo :: Number -> Number
cubo numero = numero ^ 3

--- Ejercicio 4 ---
area :: Number -> Number -> Number
area base altura = base * altura

--- Ejercicio 5 ---
esBisiesto :: Number -> Bool
esBisiesto año = año `esMultiploDe` 400 || (año `esMultiploDe` 4 && not (año `esMultiploDe` 100))

--- Ejercicio 6 ---
celsiusToFahr :: Number -> Number
celsiusToFahr gradosF = (gradosF * 9 / 5) + 32

--- Ejercicio 7 ---
fahrToCelsius :: Number -> Number
fahrToCelsius gradosC = (gradosC - 32) * 5 / 9

--- Ejercicio 8 ---
haceFrio :: Number -> Bool
haceFrio x = fahrToCelsius x < 8

--- Ejercicio 9 ---
mcd :: Number -> Number -> Number
mcd a b = (a * b) / gcd a b

--- Ejercicio 10 ---
dispersion :: Number -> Number -> Number -> Number
dispersion d1 d2 d3 = max (max d1 d2) d3 - min (min d1 d2) d3

dispersion2 :: Number -> Number -> Number -> Number
dispersion2 d1 d2 d3 = (max d1 . max d2 $ d3) - (min d1 . min d2 $ d3)

diasParejos :: Number -> Number -> Number -> Bool
diasParejos d1 d2 d3 = (< 30) (dispersion d1 d2 d3)

diasLocos :: Number -> Number -> Number -> Bool
diasLocos d1 d2 d3 = (> 100) (dispersion d1 d2 d3)

diasNormales :: Number -> Number -> Number -> Bool
diasNormales d1 d2 d3 = not (diasLocos d1 d2 d3 || diasParejos d1 d2 d3)

--- Ejercicio 11 ---
basePino :: Number
basePino = 300

pesoPino :: Number -> Number
pesoPino alturaCm
  | alturaCm >= basePino = (basePino * 3) + (alturaCm - basePino) * 2
  | alturaCm < 300 && alturaCm > 0 = alturaCm * 3
  | otherwise = 0

pesoUtil :: Number -> Bool
pesoUtil peso = peso >= 400 && peso <= 1000

sirvePino :: Number -> Bool
sirvePino = pesoUtil . pesoPino

--- Ejercicio 12 --
esCuadradoAux objetivo suma paso
  | suma == objetivo = True
  | suma > objetivo = False
  | otherwise = esCuadradoAux objetivo (suma + (2*paso+1)) (paso +1)

esCuadradoPerfecto n = esCuadradoAux n 0 0

