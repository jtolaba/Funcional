module Library where
import PdePreludat
import Test.Hspec (xcontext)

doble :: Number -> Number
doble numero = numero + numero
--- Ejercicio 1 ---
siguiente ::Number -> Number
siguiente = (+1)

--- Ejercicio 2 ---
mitad :: Number -> Number
mitad =  (/2)

--- Ejercicio 3 ---
inversa :: Number -> Number
inversa = (1/)

--- Ejercicio 4 ---
triple :: Number -> Number
triple =(*3)

--- Ejercicio 5 ---
esNumeroPositivo :: Number -> Bool
esNumeroPositivo = (>0)

--- Ejercicio 6 ---
esMultiploDe :: Number -> Bool
esMultiploDe = (0==). (`mod` 2)

--- Ejercicio 7 ---
esBisiesto = ()
