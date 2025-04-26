{-# OPTIONS_GHC -Wno-missing-fields #-}

module Library where

import GHC.Base (ap, augment)
import PdePreludat
import GHC.ExecutionStack (Location(Location))

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

data Tramo = Curva Angulo Longitud | Recta Longitud | ZigZag CambiosDireccion | Rulo DiametroRulo deriving (Show)

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
desgasteRuedas auto = fst (desgaste auto)

desgasteChasis :: Auto -> Chasis
desgasteChasis auto = snd (desgaste auto)

cantidadDeApodos :: Auto -> Number
cantidadDeApodos auto = length (apodo auto)

cantidadLetrasModelo :: Auto -> Number
cantidadLetrasModelo auto = length (modelo auto)

cantidadLetrasPrimerApodo :: Auto -> Number
cantidadLetrasPrimerApodo auto = length (head (apodo auto))


-- Punto 2a
esPeugeot :: Auto -> Bool
esPeugeot auto = marca auto == "Peugeot"

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto = not (esPeugeot auto) && cumpleCondicionesDeBuenEstado auto
cumpleCondicionesDeBuenEstado auto =
    (tiempoDeCarrera auto < 100 && desgasteChasis auto < 20) ||
    (tiempoDeCarrera auto > 100 && desgasteChasis auto < 40 && desgasteRuedas auto < 60)

arrancaConLa :: Auto -> Bool
arrancaConLa auto = (take 2 . head  . apodo) auto == "La"

-- Punto 2b
noDaMas :: Auto -> Bool
noDaMas auto =
    (> 80) (desgasteChasis auto) && arrancaConLa auto ||
    (> 80) (desgasteRuedas auto) && not (arrancaConLa auto)

-- Punto 2c
esUnChiche :: Auto -> Bool
esUnChiche auto =
    (< 20) (desgasteChasis auto) && (esPar . cantidadDeApodos) auto ||
    (< 50) (desgasteChasis auto) && (not . esPar . cantidadDeApodos) auto

-- Punto 2d
esUnaJoya :: Auto -> Bool
esUnaJoya auto =
    (== 0) (desgasteRuedas auto) &&
    (== 0) (desgasteChasis auto) &&
    cantidadDeApodos auto == 1

-- Punto 2e
nivelChetez :: Auto -> Number
nivelChetez auto = 20 * cantidadDeApodos auto * cantidadLetrasModelo auto

-- Punto 2f
capacidadSupercalifragilisticaespialidosa :: Auto -> Number
capacidadSupercalifragilisticaespialidosa = cantidadLetrasPrimerApodo

-- Punto 2g
-- Calcular qué tan riesgoso es un auto. Esto es igual a la velocidad máxima por un décimo del desgaste en las ruedas.
-- Y, si el auto no está en buen estado, es el doble.

valorDeRiesgoAuto :: Auto -> Number
valorDeRiesgoAuto auto
  | estaEnBuenEstado auto = riesgoBase
  | otherwise = (*2) riesgoBase
  where riesgoBase = velocidadMaxima auto * (desgasteRuedas auto / 10)

-- Punto 3a
-- Reparar un Auto la reparación de un auto baja en un 85% el
-- desgaste del chasis (es decir que si está en 50, lo baja a 7.5) y deja en 0 el desgaste de las ruedas.

reducirDesgasteChasis :: Number -> Number
reducirDesgasteChasis desgaste = desgaste * 0.15

repararUnAuto :: Auto -> Auto
repararUnAuto auto = auto {desgaste = (0, reducirDesgasteChasis (desgasteChasis auto))}

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
curvaPeligrosa = Curva 60 300
curvaTranca = Curva 110 550
tramoRectroClassic = Recta 715
tramito = Recta 260
zigZagLoco = ZigZag 5
casiCurva = ZigZag 1
ruloClasico = Rulo 13
deseoDeMuerte = Rulo 26

transitarTramo :: Tramo -> Auto -> Auto
transitarTramo tramo = aumentarDesgaste tramo . sumarTiempoEnPista tramo

sumarTiempoEnPista :: Tramo -> Auto -> Auto
sumarTiempoEnPista tramo auto = auto {tiempoDeCarrera = tiempoActual + tiempoDeTramo}
  where
    tiempoActual = tiempoDeCarrera auto
    tiempoDeTramo = case tramo of
      Curva angulo longitud -> longitud / (velocidadMaxima auto / 2)
      Recta longitud -> longitud / velocidadMaxima auto
      ZigZag cambios -> cambios * 3
      Rulo diametro -> 5 * diametro / velocidadMaxima auto

aumentarDesgaste :: Tramo -> Auto -> Auto
aumentarDesgaste tramo auto = auto {desgaste = desgastePorTramo}
  where
    desgasteRuedaActual = desgasteRuedas auto
    desgasteChasisActual = desgasteChasis auto
    desgastePorTramo :: (Ruedas, Chasis)
    desgastePorTramo = case tramo of
      Curva angulo longitud -> (desgasteRuedaActual + (longitud * 3) / angulo, desgasteChasisActual)
      Recta longitud -> (desgasteRuedaActual, desgasteChasisActual + longitud / 100)
      ZigZag cambios -> (desgasteRuedaActual + (velocidadMaxima auto * cambios) / 10, desgasteChasisActual + 5)
      Rulo diametro -> (desgasteRuedaActual + diametro * 1.5, desgasteChasisActual)

-- Punto 5a
-- esUnaJoya :: Auto -> Bool

nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez [] = 0
nivelDeJoyez (auto:autos)
  | esUnaJoya auto && tiempoDeCarrera auto < 50 = 2 + nivelDeJoyez autos
  | esUnaJoya auto                              = 1 + nivelDeJoyez autos
  | otherwise                                   = nivelDeJoyez autos

--ola
-- Punto 5b

sonParaEntendidos :: [Auto] -> Bool
sonParaEntendidos [] = True
sonParaEntendidos (x:xs) = estaEnBuenEstado x && tiempoDeCarrera x <= 200 && sonParaEntendidos xs
