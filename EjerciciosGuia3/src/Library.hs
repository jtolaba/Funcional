{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Evaluate" #-}
module Library where

import PdePreludat
import Test.Hspec (xcontext)

doble :: Number -> Number
doble numero = numero + numero

--- Ejercicio 1 ---

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

std3 :: (a, b, c) -> b
std3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

--- Ejercicio 2 ---
aplicar :: (a -> b, a -> c) -> a -> (b, c)
aplicar (f, g) x = (f x, g x)

--- Ejercicio 3 ---
cuentaBizarra :: (Number, Number) -> Number
cuentaBizarra (a, b)
  | a > b = a + b
  | mayorQueDiez = b - a
  | not mayorQueDiez && b > a = a * b
  where
    mayorQueDiez = (b - a) > 10

--- Ejercicio 4 ---
type Parcial = (Number, Number)

type NotaFinal = Parcial

esNotaBochazo :: Number -> Bool
esNotaBochazo = (< 6)

aprobo :: Parcial -> Bool
aprobo (a, b) = (not . esNotaBochazo) a && (not . esNotaBochazo) b

promociono :: Parcial -> Bool
promociono (a, b) = aprobo (a, b) && a + b >= 14

aproboPrimerParcial :: Parcial -> Bool
aproboPrimerParcial = not . esNotaBochazo . fst

--- Ejercicio 5 ---
notasFinales :: (Parcial, Parcial) -> NotaFinal
notasFinales (a, b) = (max (fst a) (fst b), max (snd a) (snd b))

recursa = not . aprobo . notasFinales

recuperoPrimerParcial (a, b) = not $ (== -1) (fst b)

recuperoPorGusto :: (Parcial, Parcial) -> Bool
recuperoPorGusto (a, b) = promociono a && (fst b /= -1) || (snd b /= -1)

--- Ejercicio 6 ---
esMayorDeEdad :: (String, Number) -> Bool
esMayorDeEdad = (> 18) . snd

--- Ejercicio 7 ---
duplicar :: Bool -> Number -> Number
duplicar True  a = a * 2
duplicar False a = a
sumarUno :: Bool->Number->Number
sumarUno True a = a + 1
sumarUno False a = a

calcular :: (Number, Number) -> (Number, Number)
calcular (a, b) = (((`duplicar` a) . even) a, ((`sumarUno` b) . odd) b)