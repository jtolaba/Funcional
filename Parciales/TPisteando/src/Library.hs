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
  (((> 100) . tiempoDeCarrera $ auto) && ((< 40) . desgasteChasis $ auto) && ((< 60) . desgasteRuedas $ auto))

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
curvaPeligrosa :: Tramo
curvaPeligrosa = Curva 60 300

curvaTranca :: Tramo
curvaTranca = Curva 110 550

tramoRetroClassic :: Tramo
tramoRetroClassic = Recta 715

tramito :: Tramo
tramito = Recta 260

zigZagLoco :: Tramo
zigZagLoco = ZigZag 5

casiCurva :: Tramo
casiCurva = ZigZag 1

ruloClasico :: Tramo
ruloClasico = Rulo 13

deseoDeMuerte :: Tramo
deseoDeMuerte = Rulo 26

data Tramo = Curva Angulo Longitud | Recta Longitud | ZigZag CambiosDireccion | Rulo DiametroRulo deriving (Show)

transitarTramo :: Tramo -> Auto -> Auto
transitarTramo tramo = aumentarTiempo tramo . aumentarDesgaste tramo

aumentarTiempo :: Tramo -> Auto -> Auto
aumentarTiempo (Curva angulo longitud) auto = auto {tiempoDeCarrera = tiempoDeCarrera auto + (longitud / (velocidadMaxima auto / 2))}
aumentarTiempo (Recta longitud) auto = auto {tiempoDeCarrera = tiempoDeCarrera auto + (longitud / velocidadMaxima auto)}
aumentarTiempo (ZigZag cambios) auto = auto {tiempoDeCarrera = tiempoDeCarrera auto + (cambios * 3)}
aumentarTiempo (Rulo diametro) auto = auto {tiempoDeCarrera = tiempoDeCarrera auto + ((5 * diametro) / velocidadMaxima auto)}

aumentarDesgaste :: Tramo -> Auto -> Auto
aumentarDesgaste (Curva angulo longitud) auto = auto {desgaste = (desgasteRuedas auto + (3 * longitud / angulo), desgasteChasis auto)}
aumentarDesgaste (Recta longitud) auto = auto {desgaste = (desgasteRuedas auto, desgasteChasis auto + longitud / 100)}
aumentarDesgaste (ZigZag cambios) auto = auto {desgaste = (desgasteRuedas auto + velocidadMaxima auto * cambios / 10, desgasteChasis auto + 5)}
aumentarDesgaste (Rulo diametro) auto = auto {desgaste = (desgasteRuedas auto + diametro * 1.5, desgasteChasis auto)}

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
alpine = Equipo "Alpine" 3000 [ferrari,fiat,fiat]
williams = Equipo "Red Bull" 4500 [lamborghini,peugeot]
mercedes = Equipo "Mercedes" 20000 [fiat,peugeot]

realizarDescuentoPresupuesto :: Number -> Equipo -> Number
realizarDescuentoPresupuesto descuento equipo = presupuesto equipo - descuento

--- agregarAutoAEquipo: añade un un auto y realiza un descuento en el presupuesto del equipo. 
--- Tipo de descuento: costo de inscripcion del auto = 1000 * velocidad maxima del auto


--- realizarReparacionEnEquipo: repara un auto realiza el descuento en el presupuesto del equipo
--- Tipo de descuento: 500 * por cada punto de desgaste del chasis
--- Recursividad: se repara todos los autos del equipo mientras el presupuesto lo permita


--- optimazarAutosEquipo:
--- Tipo de descuento: velocidad maxima (inicial) * 100
--- Recursividad: se pone nitro a todos los autos del equipo mientras el presupuesto lo permita

--- ferrarizar: lleva al desarmadero todos los autos del equipo y les cambia la marca a "Ferrari" y el modelo a "F50"
--- Tipo de descuento: costo de convertir un auto en ferrari = 3500
--- Recursividad: realiza la operación mientras el presupuesto lo permita
--- Nota: Un Auto ferrari no puede ser llevado al desarmadero es decir queda igual.

ferrarizar ::Equipo -> Equipo

  where
