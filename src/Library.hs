module Library where
import Data.Ratio (numerator)
import PdePreludat


doble numero = numero + numero
esMultiploDeTres x = mod x 3 == 0
esMultiploDe numero divisor = mod numero divisor == 0
cubo = (^3)
potencia =(^)
area = (*)
esBisiesto año = año `esMultiploDe` 400 || (año `esMultiploDe` 4 && not (año `esMultiploDe` 100))

celsiusToFahr grados = (grados * 9 / 5) + 32

fahrToCelsius grados = (grados - 32) * 5 / 9

haceFrio x = fahrToCelsius x < 8

mcd a b = (a * b) / gcd a b

dispersion d1 d2 d3 = max (max d1 d2) d3 - min (min d1 d2) d3

