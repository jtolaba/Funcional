module Library where

import Data.Ratio (numerator)
import GHC.Base (List)
import PdePreludat
import Test.Hspec (xcontext)
import GHC.Num (Num)


doble :: Number -> Number
doble = (*2)

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
dispersion d1 d2 d3 = max (max d1 d2) d3 - min (min d1 d2)d3

dispersion2 :: Number -> Number -> Number -> Number
dispersion2 d1 d2 d3 = (max d1 . max d2 $d3 )- (min d1 . min d2 $d3)

diasParejos :: Number -> Number -> Number -> Bool
diasParejos d1 d2 d3 = (<30) (dispersion d1 d2 d3)

diasLocos :: Number -> Number -> Number -> Bool
diasLocos d1 d2 d3 = (>100) (dispersion d1 d2 d3)

diasNormales :: Number -> Number -> Number -> Bool
diasNormales d1 d2 d3 = not ((diasLocos d1 d2 d3) || (diasParejos d1 d2 d3))