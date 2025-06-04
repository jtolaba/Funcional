module Library where

import PdePreludat



----Modelo de autos---
ferrari = Auto "Ferrari" "F50" (0,0) 65 0 ["La nave", "El fierro", "Ferrucho"]
lamborghini = Auto "Lamborghini" "Diablo" (4,7) 73 0 ["Lambo", "La bestia"]
fiat = Auto "Fiat" "600" (27,33) 44 0 ["La Bocha", "La bolita", "Fitito"]
peugeot = Auto "Peugeot" "504" (0,0) 40 0 ["El rey del desierto"]


-- Defincion de datos
type Marca = String

type Modelo = String

type Apodo = String

type Ruedas = Number

type Chasis = Number

type Tiempo = Number

type Nombre = String

type Pais = String

type Longitud = Number

type Angulo = Number

type CambiosDireccion = Number

type DiametroRulo = Number

type PrecioBaseEntrada = Number

data Auto = Auto
  { marca :: Marca,
    modelo :: Modelo,
    desgaste :: (Ruedas, Chasis),
    velocidadMaxima :: Number,
    tiempoDeCarrera :: Tiempo, -- Inicialmente 0
    apodo :: [Apodo]
  }
  deriving (Show)

data Pista = Pista
  { nombre :: Nombre,
    pais :: Pais,
    precioBaseEntrada :: PrecioBaseEntrada,
    tramos :: [Tramo]
  }
  deriving (Show)

-- Funciones de uso general
esPar :: Number -> Bool
esPar = even

desgasteRuedas :: Auto -> Ruedas
desgasteRuedas = fst . desgaste

desgasteChasis :: Auto -> Chasis
desgasteChasis = snd . desgaste

cantidadDeApodos :: Auto -> Number
cantidadDeApodos = length . apodo

cantidadLetrasModelo :: Auto -> Number
cantidadLetrasModelo = length . modelo

cantidadLetrasPrimerApodo :: Auto -> Number
cantidadLetrasPrimerApodo = length . head . apodo

-- Punto 2a
esPeugeot :: Auto -> Bool
esPeugeot auto = marca auto == "Peugeot"

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto = (not . esPeugeot) auto && cumpleCondicionesDeBuenEstado auto

cumpleCondicionesDeBuenEstado auto =
  (((< 100) . tiempoDeCarrera $ auto) && ((< 20) . desgasteChasis $ auto))  ||
  (((>= 100) . tiempoDeCarrera $ auto) && ((< 40) . desgasteChasis $ auto) && ((< 60) . desgasteRuedas $ auto))

-- (tiempoDeCarrera auto < 100 && desgasteChasis auto < 20) ||
-- (tiempoDeCarrera auto > 100 && desgasteChasis auto < 40 && desgasteRuedas auto < 60)

arrancaConLa :: Auto -> Bool
arrancaConLa = (== "La") . take 2 . head . apodo

-- Punto 2b
noDaMas :: Auto -> Bool
noDaMas auto =
  (((> 80) . desgasteChasis $ auto) && arrancaConLa auto) ||
  (((> 80) . desgasteRuedas $ auto) && (not . arrancaConLa $ auto))

-- Punto 2c
esUnChiche :: Auto -> Bool
esUnChiche auto =
  ((< 20) . desgasteChasis $ auto) && (esPar . cantidadDeApodos $ auto) ||
  ((< 50) . desgasteChasis $ auto) && (not . esPar . cantidadDeApodos $ auto)

-- Punto 2d
esUnaJoya :: Auto -> Bool
esUnaJoya auto =
  ((== 0) . desgasteRuedas $ auto) &&
  ((== 0) . desgasteChasis $ auto) &&
  cantidadDeApodos auto == 1

-- Punto 2e
nivelChetez :: Auto -> Number
nivelChetez auto = (* 20) . (* cantidadDeApodos auto) . cantidadLetrasModelo $ auto

-- Punto 2f
capacidadSupercalifragilisticaespialidosa :: Auto -> Number
capacidadSupercalifragilisticaespialidosa = cantidadLetrasPrimerApodo

-- Punto 2g
-- Calcular qué tan riesgoso es un auto. Esto es igual a la velocidad máxima por un décimo del desgaste en las ruedas.
-- Y, si el auto no está en buen estado, es el doble.

valorDeRiesgoAuto :: Auto -> Number
valorDeRiesgoAuto auto
  | estaEnBuenEstado auto = riesgoBase
  | otherwise = (* 2) riesgoBase
  where
    riesgoBase = velocidadMaxima auto * (desgasteRuedas auto / 10)

-- Punto 3a
-- Reparar un Auto la reparación de un auto baja en un 85% el
-- desgaste del chasis (es decir que si está en 50, lo baja a 7.5) y deja en 0 el desgaste de las ruedas.

reducirDesgasteChasis :: Number -> Number
reducirDesgasteChasis = (* 0.15)

repararUnAuto :: Auto -> Auto
repararUnAuto auto = auto {desgaste = (0, reducirDesgasteChasis . desgasteChasis $ auto)}

-- Punto 3b
aplicarPenalidad :: Auto -> Tiempo -> Auto
aplicarPenalidad auto x = auto {tiempoDeCarrera = tiempoDeCarrera auto + x}

-- Punto 3c
ponerNitro :: Auto -> Auto
ponerNitro auto = auto {velocidadMaxima = velocidadMaxima auto * 1.2}

-- Punto 3d
bautizarAuto :: Auto -> Apodo -> Auto
bautizarAuto auto nuevoApodo = auto {apodo = apodo auto ++ [nuevoApodo]}

-- Punto 3e
llevarAlDesarmadero :: Auto -> Marca -> Modelo -> Auto
llevarAlDesarmadero auto marca modelo = auto {marca = marca, modelo = modelo, apodo = ["Nunca Taxi"]}

-- Punto 4a
--- Modelo de tramos ---
curvaPeligrosa :: TipoTramo
curvaPeligrosa = Curva 60 300

curvaTranca :: TipoTramo
curvaTranca = Curva 110 550

tramoRetroClassic :: TipoTramo
tramoRetroClassic = Recta 715

tramito :: TipoTramo
tramito = Recta 260

zigZagLoco :: TipoTramo
zigZagLoco = ZigZag 5

casiCurva :: TipoTramo
casiCurva = ZigZag 1

ruloClasico :: TipoTramo
ruloClasico = Rulo 13

deseoDeMuerte :: TipoTramo
deseoDeMuerte = Rulo 26

data TipoTramo = Curva Angulo Longitud | Recta Longitud | ZigZag CambiosDireccion | Rulo DiametroRulo deriving (Show)

type Tramo = Auto -> Auto

transitarTramo :: TipoTramo -> Auto -> Auto
transitarTramo tramo = aumentarTiempo tramo . aumentarDesgaste tramo

type ModificadorDeTiempo = Number -> Number

alterarTiempo :: ModificadorDeTiempo -> Auto -> Auto
alterarTiempo modificador auto = auto { tiempoDeCarrera = (modificador . tiempoDeCarrera) auto}


aumentarTiempo :: TipoTramo -> Auto -> Auto
aumentarTiempo (Curva angulo longitud) auto = alterarTiempo (+tiempoCurva) auto
  where tiempoCurva = longitud / (velocidadMaxima auto / 2)

aumentarTiempo (Recta longitud) auto = alterarTiempo (+ tiempoRecta) auto
  where tiempoRecta = longitud / velocidadMaxima auto

aumentarTiempo (ZigZag cambios) auto = alterarTiempo (+ tiempoZigZag) auto
  where tiempoZigZag = cambios * 3

aumentarTiempo (Rulo diametro) auto = alterarTiempo (+ tiempoRulo) auto
  where tiempoRulo = (5 * diametro) / velocidadMaxima auto

type ModificadorDesgaste = (Number,Number) -> (Number,Number)

alterarDesgaste :: ModificadorDesgaste ->Auto->Auto
alterarDesgaste modificadorDesgaste auto = auto { desgaste = (modificadorDesgaste . desgaste) auto}

aumentarDesgaste :: TipoTramo -> Auto -> Auto

aumentarDesgaste (Curva angulo longitud) auto = alterarDesgaste modificar auto
  where
    modificar (ruedas, chasis) = (ruedas + (3 * longitud / angulo), chasis)

aumentarDesgaste (Recta longitud) auto = alterarDesgaste modificar auto
  where
    modificar (ruedas, chasis) = (ruedas, chasis + longitud / 100)

aumentarDesgaste (ZigZag cambios) auto = alterarDesgaste modificar auto
  where
    v = velocidadMaxima auto
    modificar (ruedas, chasis) = (ruedas + v * cambios / 10, chasis + 5)

aumentarDesgaste (Rulo diametro) auto = alterarDesgaste modificar auto
  where
    modificar (ruedas, chasis) = (ruedas + diametro * 1.5, chasis)

type ModificadorVelocidad = Number -> Number

alterarVelocidad :: ModificadorVelocidad -> Auto -> Auto
alterarVelocidad modificador auto = auto {velocidadMaxima = (modificador . velocidadMaxima) auto}


-- aumentarDesgaste (Curva angulo longitud) auto = auto {desgaste = (desgasteRuedas auto + (3 * longitud / angulo), desgasteChasis auto)}
-- aumentarDesgaste (Recta longitud) auto = auto {desgaste = (desgasteRuedas auto, desgasteChasis auto + longitud / 100)}
-- aumentarDesgaste (ZigZag cambios) auto = auto {desgaste = (desgasteRuedas auto + velocidadMaxima auto * cambios / 10, desgasteChasis auto + 5)}
-- aumentarDesgaste (Rulo diametro) auto = auto {desgaste = (desgasteRuedas auto + diametro * 1.5, desgasteChasis auto)}


-- Punto 5a
-- esUnaJoya :: Auto -> Bool

nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez [] = 0
nivelDeJoyez (auto : autos)
  | esUnaJoya auto && tiempoDeCarrera auto < 50 = 2 + nivelDeJoyez autos
  | esUnaJoya auto = 1 + nivelDeJoyez autos
  | otherwise = nivelDeJoyez autos

-- Punto 5b

sonParaEntendidos :: [Auto] -> Bool
sonParaEntendidos [] = True
sonParaEntendidos (x : xs) = estaEnBuenEstado x && tiempoDeCarrera x <= 200 && sonParaEntendidos xs

--------------------------------- Segunda parte del TP ---------------------------------

data Equipo = Equipo
  { nombreEquipo :: Nombre,
    presupuesto :: Number,
    autos :: [Auto]
  }
  deriving (Show)
alpine = Equipo "Alpine" 20000 [
  Auto "Ferrari" "F50" (10,10) 65 10 ["La nave"],
  Auto "Lamborghini" "Diablo" (10,20) 65 10 ["La nave"]
  ]
williams = Equipo "Red Bull" 4500 [lamborghini,peugeot]
mercedes = Equipo "Mercedes" 20000 [fiat,peugeot]



--- Punto 4
--- 4.a ---
boxes trayectoria tipoTramo auto
  | noDaMas auto          = auto  -- No puede hacer nada si no da más
  | estaEnBuenEstado auto = trayectoria tipoTramo auto
  | otherwise             = repararUnAuto . alterarTiempo (+10) $ trayectoria tipoTramo auto

--- 4.b ---
mojado trayectoria tipoTramo auto= alterarTiempo (+tiempoExtra) $ trayectoria tipoTramo auto
  where tiempoExtra = tiempoDeCarrera (trayectoria tipoTramo auto) / 2

--- 4.c ---
ripio trayectoria tipoTramo = pasarPorTramo tipoTramo . trayectoria tipoTramo

--- 4.d ---
obstruccion metros trayectoria tipoTramo = alterarDesgaste modificar $ trayectoria tipoTramo
  where
    desgasteRuedasPorObstruccion = metros * 2
    modificar (ruedas,chasis) = (ruedas + desgasteRuedasPorObstruccion, chasis)

--- 4.e ---
turbo trayectoria tipoTramo auto = alterarVelocidad (\ _ -> velocidadMaxima auto) . trayectoria tipoTramo . alterarVelocidad (*2)$auto

--- Punto 5
pasarPorTramo :: TipoTramo -> Auto -> Auto
pasarPorTramo tramo auto
  | (not . noDaMas) auto = transitarTramo tramo auto
  | otherwise = auto

--- Punto 6
--- 6.a ---
vueltaALaManzana :: Pista
vueltaALaManzana = Pista "La Manzana" "Italia" 30 [
  pasarPorTramo tramoA,
  pasarPorTramo tramoB,
  pasarPorTramo tramoA,
  pasarPorTramo tramoB,
  pasarPorTramo tramoA,
  pasarPorTramo tramoB,
  pasarPorTramo tramoA,
  pasarPorTramo tramoB
  ]
    where
    tramoA = Recta 130
    tramoB = Curva 90 13

--- 6.b ---
superPista :: Pista
superPista= Pista "SuperPista" "Argentina" 300 [
  pasarPorTramo tramoRetroClassic,
  pasarPorTramo curvaTranca,
  turbo pasarPorTramo tramito, ---modificado tiene turbo
  mojado pasarPorTramo tramito, ---modificado esta mojado
  pasarPorTramo (Rulo 10),
  (obstruccion 2 . pasarPorTramo) (Curva 80 400), ---modificado con obstrucción de 2m
  pasarPorTramo (Curva 115 650),
  pasarPorTramo (Recta 970),
  pasarPorTramo curvaPeligrosa,
  ripio pasarPorTramo tramito, --- modificado con ripio
  boxes pasarPorTramo (Recta 800),
  (obstruccion 5 . pasarPorTramo) casiCurva,
  pasarPorTramo (ZigZag 2),
  (ripio . mojado) pasarPorTramo deseoDeMuerte,
  pasarPorTramo ruloClasico,
  pasarPorTramo zigZagLoco
  ]


--- 6.c ---
recorrerTramosConAuto :: [Auto -> Auto] -> Auto -> Auto
recorrerTramosConAuto tramos auto = foldl (\a f -> f a) auto tramos

recorrerTramosConAuto' = foldl (flip ($))
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta pista = map (recorrerTramosConAuto (tramos pista))

ferrariTest :: Auto
ferrariTest = Auto "Ferrari" "F50" (79,80) 65 0 ["La nave"]
peugeotTest = Auto "Peugeot" "504" (79,0) 40 0 ["El rey del desierto"]
-- Auto "Ferrari" "F50" (10,82.6) 65 0 ["La nave"] 0

--- Punto 7
--- 7.a ---
data Carrera = Carrera{
  pista :: Pista,
  vueltas :: Number
} deriving (Show)

--- 7.b ---
tourDeBuenosAires = Carrera superPista 20

--- 7.c ---
simularVueltas :: Number -> Pista -> [Auto] -> [[Auto]]
simularVueltas 0 _ autos = [autos]
simularVueltas _ _ [] = []
simularVueltas n pista autos =
  autos : simularVueltas (n - 1) pista (filter (not . noDaMas) (map (recorrerTramosConAuto (tramos pista)) autos))

simularCarrera :: Carrera -> [Auto] -> [[Auto]]
simularCarrera carrera autos = simularVueltas (vueltas carrera) (pista carrera) autos


-- Funcion dada en el enunciado
quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy valoracion (x:xs) = anteriores ++ [x] ++ posteriores    
    where
        anteriores  = quickSortBy valoracion $ filter ((< valoracion x).valoracion)  xs
        posteriores = quickSortBy valoracion $ filter ((>= valoracion x).valoracion) xs
-- recorrerVueltas :: Number -> [Auto] -> [Auto]
-- recorrerVueltas 0 autos = autos
-- recorrerVueltas vueltas autos
--   | null autos = []
--   | otherwise = recorrerVueltas (vueltas - 1) (filter (not.noDaMas) (peganLaVuelta (pista carrera) autos))
--   where
--     carrera = Carrera superPista vueltas

-- quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
-- quickSortBy _ [] = []
-- quickSortBy valoracion (x:xs) = anteriores ++ [x] ++ posteriores    
--     where
--         anteriores  = quickSortBy valoracion $ filter ((< valoracion x).valoracion)  xs
--         posteriores = quickSortBy valoracion $ filter ((>= valoracion x).valoracion) xs
