module Library where

import PdePreludat

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada y definir funciones para resolver los siguientes problemas:
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data Auto = Auto
  { color :: Color,
    velocidad :: Number,
    distanciaRecorrida :: Number
  }
  deriving (Eq, Show)

type Carrera = [Auto]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Datos de Ejemplo
amarillo = Auto "amarillo" 120 100

verde = Auto "verde" 100 95

azul = Auto "azul" 100 120

rojo = Auto "rojo" 160 200

testCarrera = [amarillo, verde, azul, rojo]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- a) Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaEntre auto1 auto2 < 10

distanciaEntre :: Auto -> Auto -> Number
distanciaEntre auto1 = abs . (distanciaRecorrida auto1 -) . distanciaRecorrida

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- b) Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (not . tieneAlgunoCerca auto) carrera && vaGanando auto carrera

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto = all (leVaGanando auto) . contrincantes auto

contrincantes :: Auto -> Carrera -> [Auto]
contrincantes auto = filter (/= auto)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando ganador auto = ganador /= auto && distanciaRecorrida ganador > distanciaRecorrida auto

tieneAlgunoCerca :: Auto -> Carrera -> Bool
tieneAlgunoCerca auto carrera = any (estaCerca auto) (filter (/= auto) carrera)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- c) Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
puesto :: Auto -> Carrera -> Number
puesto auto = (+ 1) . length . filter (not . leVaGanando auto) . contrincantes auto

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- 2.a Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
correr :: Number -> Auto -> Auto
correr tiempo auto = auto {distanciaRecorrida = distanciaRecorrida auto + tiempo * velocidad auto}

type ModificadorVelocidad = Number -> Number

alterarVelocidad :: ModificadorVelocidad -> Auto -> Auto
alterarVelocidad modificador auto = auto {velocidad = (modificador . velocidad) auto}

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad velocidadABajar = alterarVelocidad (max 0 . subtract velocidadABajar)

afectarALosQueCumplen :: (Auto -> Bool) -> (Auto -> Auto) -> [Auto] -> [Auto]
afectarALosQueCumplen criterio efecto lista =
  (map efecto . filter criterio) lista ++ filter (not . criterio) lista

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto autoQueGatillo = afectarALosQueCumplen (estaCerca autoQueGatillo) (bajarVelocidad 50)

miguelitos :: Number -> PowerUp
miguelitos velocidadAReducir autoQueGatillo = afectarALosQueCumplen (not . leVaGanando autoQueGatillo) (bajarVelocidad velocidadAReducir)

jetPack :: Number -> PowerUp
jetPack tiempo autoQueGatillo =
    afectarALosQueCumplen (== autoQueGatillo) (alterarVelocidad (\ _ -> velocidad autoQueGatillo) . correr tiempo . alterarVelocidad (*2))

-- (alterarVelocidad (\ _ ->velocidad amarillo) . correr 10 . bajarVelocidad 120) amarillo
type Color = String
type Evento = Carrera -> Carrera
simularCarrera :: Carrera -> [Evento] -> [(Number, Color)]
simularCarrera carrera eventos = (tablaDePosiciones . procesarEventos eventos) carrera

tablaDePosiciones :: Carrera -> [(Number, Color)]
tablaDePosiciones carrera 
  = map (entradaDeTabla carrera) carrera

entradaDeTabla :: Carrera -> Auto -> (Number, String)
entradaDeTabla carrera auto = (puesto auto carrera, color auto)

tablaDePosiciones' :: Carrera -> [(Number, String)]
tablaDePosiciones' carrera 
  = zip (map (flip puesto carrera) carrera) (map color carrera)

procesarEventos :: [Evento] -> Carrera -> Carrera
procesarEventos eventos carreraInicial =
    foldl (\carreraActual evento -> evento carreraActual) 
      carreraInicial eventos

procesarEventos' eventos carreraInicial =
    foldl (flip ($)) carreraInicial eventos

correnTodos :: Number -> Evento
correnTodos tiempo = map (correr tiempo)

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp colorBuscado carrera =
    powerUp autoQueGatillaElPoder carrera
    where autoQueGatillaElPoder = find ((== colorBuscado).color) carrera

find :: (c -> Bool) -> [c] -> c
find cond = head . filter cond

ejemploDeUsoSimularCarrera =
    simularCarrera autosDeEjemplo [
        correnTodos 30,
        usaPowerUp (jetPack 3) "azul",
        usaPowerUp terremoto "blanco",
        correnTodos 40,
        usaPowerUp (miguelitos 20) "blanco",
        usaPowerUp (jetPack 6) "negro",
        correnTodos 10
    ]

autosDeEjemplo :: [Auto]
autosDeEjemplo = map (\color -> Auto color 120 0) ["rojo", "blanco", "azul", "negro"]
