module Library where
import PdePreludat

-- De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), la velocidad a la que está yendo y la distancia que recorrió, ambos valores de tipo entero.

------ Modelos para las pruebas ------
amarillo = Auto "Amarillo" 120 100
verde = Auto "Verde" 100 90
azul = Auto "Azul" 100 120
rojo = Auto "Rojo" 160 200
data Auto = Auto
  {color :: String,
    velocidad :: Number,
    distancia :: Number
  } deriving (Show)

data Carrera = Carrera
  { autos::[Auto]    
  } deriving (Show)

-------------------------------------------------------- Punto 1 --------------------------------------------------------

estaCerca :: Auto -> Auto -> Bool
estaCerca a1 a2 =  ((<10) . abs$(distancia a1 - distancia a2)) && color a1 /= color a2



-------------------------------------------------------- Punto 2 --------------------------------------------------------