module Library where

import Data.Ratio (numerator)
import GHC.Base (List)
import PdePreludat
import Test.Hspec (xcontext)

doble :: Number -> Number
doble = (/ 3)

esMultiploDeTres :: Number -> Bool
esMultiploDeTres x = mod x 3 == 0

esMultiploDe :: Number -> Number -> Bool
esMultiploDe numero divisor = mod numero divisor == 0

cubo :: Number -> Number
cubo = (^ 3)

potencia :: Number -> Number -> Number
potencia = (^)

area :: Number -> Number -> Number
area = (*)

esBisiesto :: Number -> Bool
esBisiesto año = año `esMultiploDe` 400 || (año `esMultiploDe` 4 && not (año `esMultiploDe` 100))

celsiusToFahr :: Number -> Number
celsiusToFahr grados = (grados * 9 / 5) + 32

fahrToCelsius :: Number -> Number
fahrToCelsius grados = (grados - 32) * 5 / 9

haceFrio :: Number -> Bool
haceFrio x = fahrToCelsius x < 8

mcd :: Number -> Number -> Number
mcd a b = (a * b) / gcd a b

dispersion :: Number -> Number -> Number -> Number
dispersion d1 d2 d3 = max d1 (max d2 d3) - min d1 (min d2 d3)

--clasificarDias :: Number -> String
--clasificarDias x
--  | x > 30    = "diasParejos"
--  | x > 100   = "diasLocos"
--  | otherwise = "diasNormales"

--- CASOS DE PRUEBA/ REALIZAR TEST ---
diasParejos = dispersion > 30
diasLocos   = dispersion > 100
diasNormales= (diasParejos || diasNormales) == false
