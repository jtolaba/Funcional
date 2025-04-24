module Library where
import PdePreludat

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
esMultiploDe :: Number -> Number -> Bool
esMultiploDe dividendo = (==0).(dividendo `mod`)

--- Ejercicio 7 ---
-- Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100
esBisiesto :: Number -> Bool
esBisiesto año = (||) (año`esMultiploDe` 400) ((&&) (año`esMultiploDe` 4) (not (año`esMultiploDe` 100)))

esBisiesto2 :: Number -> Bool
esBisiesto2 año =  (&&) ((`esMultiploDe`4) año) (not$(`esMultiploDe`100) año)|| (`esMultiploDe`400) año

--- Ejercicio 8 ---

inversaRaizCuadrada :: Number -> Number
inversaRaizCuadrada = sqrt . inversa

--- Ejercicio 9 ---

incrementaMCuadradoN :: Number -> Number -> Number
incrementaMCuadradoN m n = (+n).(^2)$m

esResultadoPar :: Number -> Number -> Bool
esResultadoPar n m= (==0).(`mod`2).(^m)$n