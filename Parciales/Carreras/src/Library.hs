module Library where
import PdePreludat



-- De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), la velocidad a la que está yendo y la distancia que recorrió, ambos valores de tipo entero.

------ Modelos para las pruebas ------
amarillo = Auto "Amarillo" 120 100
verde = Auto "Verde" 100 98
azul = Auto "Azul" 100 120
rojo = Auto "Rojo" 160 200
data Auto = Auto
  {color :: String,
    velocidad :: Number,
    distancia :: Number
  } deriving (Eq,Show)

type Carrera =  [Auto]

testCarrera :: Carrera
testCarrera = [amarillo,verde,azul,rojo]
-------------------------------------------------------- Punto 1a --------------------------------------------------------

estaCerca :: Auto -> Auto -> Bool
estaCerca a1 a2 =  calcularDistancia a1 a2 < 10 && sonDistintos a1 a2

calcularDistancia :: Auto -> Auto -> Number
calcularDistancia a1 a2 = abs$ distancia a1 - distancia a2

sonDistintos :: Auto -> Auto -> Bool
sonDistintos a1 a2 = color a1 /= color a2
-------------------------------------------------------- Punto 1b --------------------------------------------------------

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo auto autos = (distancia auto > distancia contricanteConMayorDistanciaRecorrida) && (not . any (estaCerca auto))contricantes
  where
  contricantes = autosContricantes auto autos
  contricanteConMayorDistanciaRecorrida = autoConMayorDistaciaRecorrida contricantes

autosContricantes :: Auto -> [Auto] -> [Auto]
autosContricantes auto = filter (sonDistintos auto)

autoConMayorDistaciaRecorrida :: [Auto] -> Auto
autoConMayorDistaciaRecorrida = foldl1 mayorDistanciaRecorrida
  where
    mayorDistanciaRecorrida a1 a2
      | distancia a1 > distancia a2 = a1
      | otherwise = a2

autosPorDelante :: Auto -> [Auto] -> [Auto]
autosPorDelante a1 autos = foldl vanGanando [] (autosContricantes a1 autos)
  where
    vanGanando acumulador auto
      | distancia auto > distancia a1 = acumulador ++ [auto]
      | otherwise = acumulador

posicion :: Auto -> [Auto] -> Number
posicion auto autos = length (autosPorDelante auto autos) + 1
    
-------------------------------------------------------- Punto 1c --------------------------------------------------------


  
-------------------------------------------------------- Punto 2  --------------------------------------------------------
