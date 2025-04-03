module Library where
import PdePreludat
import Data.Ratio (numerator)

doble :: Number -> Number
doble numero = numero + numero

esMultiploDeTres :: Number -> Bool
esMultiploDeTres x = mod x 3 == 0

esMultiploDe :: Number -> Number ->Bool
esMultiploDe numero divisor = mod numero divisor == 0

cubo :: Number -> Number
cubo x = x * x * x

area :: Number ->Number -> Number
area base altura = base * altura

esBisiesto :: Number -> Bool
esBisiesto año = año `esMultiploDe` 400 || (año `esMultiploDe` 4 && not(año `esMultiploDe` 100))

celsiusToFahr :: Number -> Number
celsiusToFahr grados = (grados * 9/5 ) + 32

fahrToCelsius :: Number -> Number
fahrToCelsius grados= (grados - 32) * 5/9

haceFrio :: Number -> Bool
haceFrio x = fahrToCelsius x < 8

mcd ::Number-> Number ->  Number
mcd a b = (a * b)/ gcd a b