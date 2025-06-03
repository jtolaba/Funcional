module Library where
import PdePreludat

data Reserva = Reserva {
  pasajeros :: [String],
  tramos :: [Tramo],
  agregados :: [Agregado],
  costoBase :: Number
}deriving (Show)

data Tramo = Tramo{
  origen :: String,
  destino :: String,
  duracion :: (Number,String)
}deriving (Show)

---------------------------------------------------- Punto 2 ----------------------------------------------------
reservaLarga = Reserva
  ["Tom Borenstyn","Frank Gorek"]  --- Pasajeros
  [Tramo "Buenos Aires" "Sao Paulo" (6,"hs"), Tramo "Sao Paulo" "Londres" (10,"hs"),Tramo "Londres" "Paris" (25,"min"),Tramo "Paris" "Chascomus" (15,"min")] --- Tramos
  [lunchCompleto, menuEspecial "celiaco",otrosUtensillos ["almohada","auriculares"]] --- Agregados
  45000 --- Costo Base

reservaSinTramo = Reserva
  ["Cesar Frere"] --- Pasajeros
  [] --- Tramos
  [lunchCompleto,menuEspecial "celiaco", polizon "Jorge Tolaba", equipajeExtra 10] --- Agregados
  50000 --- Costo Base/home/jit/.local/bin/

reservaCorta = Reserva
  ["Cesar Frere"] --- Pasajeros
  [Tramo "Buenos Aires" "Chascomus" (15,"min"),Tramo "Cordoba" "Saantiago" (12,"hs")] --- Tramos
  [lunchCompleto,menuEspecial "celiaco", polizon "Jorge Tolaba", equipajeExtra 10] --- Agregados
  50000 --- Costo Base/home/jit/.local/bin/

---------------------------------------------------- Punto 3 ----------------------------------------------------

type Agregado = Reserva -> Reserva

modificarCostoBase :: (Number -> Number) -> Reserva -> Reserva
modificarCostoBase modificador reserva = reserva { costoBase = (modificador . costoBase) reserva}

lunchCompleto :: Agregado
lunchCompleto = modificarCostoBase (*1.15)

menuEspecial :: String -> Agregado
menuEspecial descripcion = modificarCostoBase (+ costoMenuEspecial)
  where
    costoMenuEspecial = length descripcion * multiplicadorDeCosto
    multiplicadorDeCosto = 10  

equipajeExtra :: Number -> Agregado
equipajeExtra cantidad = modificarCostoBase (+ costoEquipajeExtra)
  where
    costoEquipajeExtra = cantidad * costoEquipaje
    costoEquipaje = 200

otrosUtensillos :: [String] -> Agregado
otrosUtensillos lista reserva = reserva { costoBase = costoBase reserva + costoUtensillos}
  where
    costoUtensillos = (length lista * costoBase reserva)/100

polizon :: String -> Agregado
polizon pasajero reserva = reserva {pasajeros = pasajero:pasajeros reserva}

---------------------------------------------------- Punto 4 ----------------------------------------------------
-- Implementar funciones que:
-- Sume un monto al costo base de una reserva.
aumentarCostoBase :: Number -> Reserva -> Reserva
aumentarCostoBase monto = modificarCostoBase (+monto) 
-- Sume un agregado a una reserva.
sumarAgregado :: Agregado -> Reserva -> Reserva
sumarAgregado agregado reserva = reserva {agregados = agregados reserva ++ [agregado]}
-- Sume un tramo a una reserva.
añadirTramo :: Tramo -> Reserva -> Reserva
añadirTramo tramo reserva = reserva {tramos = tramos reserva ++ [tramo]}


---------------------------------------------------- Punto 5 ----------------------------------------------------

procesarAgregado :: [Agregado] -> Reserva -> Reserva
procesarAgregado agregados reserva = foldl (\reservaActual agregado -> agregado reservaActual) reserva agregados

precioFinal :: Reserva -> Reserva
precioFinal reserva = procesarAgregado (agregados reserva) reserva

---------------------------------------------------- Punto 6 ----------------------------------------------------
esUnaReservaLarga :: Reserva -> Bool
esUnaReservaLarga = (> umbralDuracionCorta) . tiempoTotalReserva
  where
    umbralDuracionCorta = 15

tiempoTotalReserva :: Reserva -> Number
tiempoTotalReserva reserva= sum . map fst $ (map pasarDeMinutosAHoras . (\a -> map duracion a) . tramos) reserva

tiempoTotalReserva' reserva= sum . map fst $ (map pasarDeMinutosAHoras . obtenerDuracion . tramos) reserva
  where
  obtenerDuracion = map duracion

pasarDeMinutosAHoras (tiempo,tipo)
  | tipo == "min" = (tiempo / 60,"hs")
  | otherwise = (tiempo,tipo)

---------------------------------------------------- Punto 7 ----------------------------------------------------
-- nuevaEscala :: Tramo -> Number -> Reserva
nuevaEscala :: Tramo -> Number -> Reserva -> Reserva
nuevaEscala tramo costo = aumentarCostoBase costo . añadirTramo tramo

---------------------------------------------------- Punto 8 ----------------------------------------------------

reservaBienConstruida :: Reserva -> Bool
reservaBienConstruida reserva = all tramoEnlazadoCorrectamente (zip (tramos reserva) ((tail.tramos) reserva)) && (not.null.tramos) reserva


tramoEnlazadoCorrectamente :: (Tramo, Tramo) -> Bool
tramoEnlazadoCorrectamente (tramoA, tramoB) = destino tramoA == origen tramoB

---------------------------------------------------- Punto 9 ----------------------------------------------------

-- a. Obtener el precio total de reservaLarga, agregándole un snack y una manta.
-- (precioFinal.sumarAgregado (otrosUtensillos ["snack","manta"])) reservaLarga

-- b. Agregarle a reservaLarga las escalas: Miami a Barcelona (11 hs., $15.000), y Barcelona a Roma (2 hs., $4.000), y saber si la reserva quedó bien construída.
-- (reservaBienConstruida . nuevaEscala (Tramo "Barcelona" "Roma" (2,"hs")) 4000 . nuevaEscala (Tramo "Miami" "Barcelona" (11,"hs")) 15000) reservaLarga 