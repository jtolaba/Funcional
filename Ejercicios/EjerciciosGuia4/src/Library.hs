module Library where
import PdePreludat
import Control.Arrow (Arrow(first))


--PARADIGMA :

doble :: Number -> Number
doble numero = numero + numero
------------------------------ EJERCICIOS CON LISTAS ------------------------------
--- Ejercicio 1 ---
sumarNumerosDeLista :: [Number]-> Number
sumarNumerosDeLista = sum

--- Ejercicio 2 ---
frecuenciaCardiaca :: [Number]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
promediofrecuenciaCardiaca :: Number
promediofrecuenciaCardiaca = sum frecuenciaCardiaca / length frecuenciaCardiaca

frecuenciaCardiacaMinuto :: Number -> Number
frecuenciaCardiacaMinuto m = frecuenciaCardiaca !! minutosAIndice m

minutosAIndice :: Number -> Number
minutosAIndice m = m/10

frecuenciaCardiacaHastaMinuto :: Number -> [Number]
frecuenciaCardiacaHastaMinuto m = take (minutosAIndice m) frecuenciaCardiaca

--- Ejercicio 3 ---
esCapicua :: [String] -> Bool
--esCapicua a = (== concat a)$(reverse . concat )a
esCapicua a =  concat a == (reverse . concat ) a

--- Ejercicio 4 ---
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

cuandoHabloMasMinutos ::((String,[Number]),(String,[Number])) -> String
cuandoHabloMasMinutos (a,b)
    | sum (snd a) > sum (snd b)  = fst a
    | sum (snd a) < sum (snd b)  = fst b

cuandoHizoMasLlamadas ::((String,[Number]),(String,[Number])) -> String
cuandoHizoMasLlamadas (a,b)
    | length (snd a) > length (snd b)  = fst a
    | length (snd a) < length (snd b)  = fst b

------------------------------ EJERCICIOS ORDEN SUPERIOR ------------------------------

--- Ejercicio 1 ---
trd3 (_,_,z)= z
snd3 (_,y,_)= y
fst3 (x,_,_)= x

existsAny :: (a -> Bool) -> (a,a,a) -> Bool
--existsAny f x = f(fst3 x) || f(snd3 x) || f(trd3 x)
existsAny f (x,y,z) = f x || f y || f z

--- Ejercicio 2 ---
triple = (* 3)
cuadrado = (^ 2)

mejor :: (a -> Number) -> (a -> Number) -> a -> Number
mejor f g a = max (f a) (g a)

--- Ejercicio 3 ---
aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar f (a, b) = (f a, f b)

--- Ejercicio 4 ---
parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c)
parDeFns f g a = (f a, g a)

------------------------------ EJERCICIOS ORDEN SUPERIOR + LISTAS ------------------------------
--- Ejercicio 1 ---
esMultilpDeAlguno :: Number->[Number]->Bool
esMultilpDeAlguno a = any ((==0). mod a)

--- Ejercicio 2 ---
