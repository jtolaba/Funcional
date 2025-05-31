module Library where
import PdePreludat



doble :: Number -> Number
doble numero = numero + numero

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

--------------------------------------------------------------- Punto 2 ---------------------------------------------------------------
reservaLarga = Reserva
  ["Tom Borenstyn","Frank Gorek"]  --- Pasajeros
  [Tramo "Buenos Aires" "Sao Paulo" (6,"hs"), Tramo "Sao Paulo" "Londres" (10,"hs"),Tramo "Buenos Aires" "Chascomus" (25,"min"),Tramo "Buenos Aires" "Chascomus" (15,"min")] --- Tramos
  [lunchCompleto, menuEspecial "celiaco",otrosUtensillos ["almohada","auriculares"]] --- Agregados
  45000 --- Costo Base

reservaCorta = Reserva
  ["Cesar Frere"] --- Pasajeros
  [Tramo "Buenos Aires" "Chascomus" (15,"min")] --- Tramos
  [lunchCompleto,menuEspecial "celiaco", polizon "Jorge Tolaba", equipajeExtra 10] --- Agregados
  50000 --- Costo Base

--------------------------------------------------------------- Punto 3 ---------------------------------------------------------------

type Agregado = Reserva -> Reserva

modificarCostoBase :: (Number -> Number) -> Reserva -> Reserva
modificarCostoBase modificador reserva = reserva { costoBase = (modificador . costoBase) reserva}

lunchCompleto :: Agregado
lunchCompleto = modificarCostoBase (*1.15)

menuEspecial :: [a] -> Agregado
menuEspecial descripcion = modificarCostoBase (+ costoMenuEspecial)
  where
    costoMenuEspecial = length descripcion * 10

equipajeExtra :: Number -> Agregado
equipajeExtra cantidad = modificarCostoBase (+ costoEquipajeExtra)
  where
    costoEquipajeExtra = cantidad * 200

otrosUtensillos :: [a] -> Agregado
otrosUtensillos lista reserva = reserva { costoBase = costoBase reserva + costoUtensillos}
  where
    costoUtensillos = (length lista * costoBase reserva)/100

polizon :: String -> Agregado
polizon pasajero reserva = reserva {pasajeros = pasajero:pasajeros reserva}

--------------------------------------------------------------- Punto 4 ---------------------------------------------------------------
-- Implementar funciones que:

-- Sume un agregado a una reserva.

-- Sume un tramo a una reserva.
añadirTramo :: Tramo -> Reserva -> Reserva
añadirTramo tramo reserva = reserva {tramos = tramos reserva ++ [tramo]}  
--------------------------------------------------------------- Punto 5 ---------------------------------------------------------------

procesarAgregado :: [Agregado] -> Reserva -> Reserva
procesarAgregado agregados reserva = foldl (\reservaActual agregado -> agregado reservaActual) reserva agregados

precioFinal :: Reserva -> Reserva
precioFinal reserva = procesarAgregado (agregados reserva) reserva

--------------------------------------------------------------- Punto 6 ---------------------------------------------------------------
esUnaReservaLarga :: Reserva -> Bool
esUnaReservaLarga = (>6) . tiempoTotalReserva

tiempoTotalReserva :: Reserva -> Number
tiempoTotalReserva reserva= sum . map fst $ (map pasarDeMinutosAHoras . (\a -> map duracion a) . tramos) reserva

tiempoTotalReserva' reserva= sum . map fst $ (map pasarDeMinutosAHoras . obtenerDuracion . tramos) reserva
  where
  obtenerDuracion = map duracion

pasarDeMinutosAHoras (tiempo,tipo)
  | tipo == "min" = (tiempo / 60,"hs")
  | otherwise = (tiempo,tipo)

--------------------------------------------------------------- Punto 7 ---------------------------------------------------------------
-- nuevaEscala :: Tramo -> Number -> Reserva
nuevaEscala :: Tramo -> Number -> Reserva -> Reserva
nuevaEscala tramo costo = modificarCostoBase (+costo) . añadirTramo tramo

--------------------------------------------------------------- Punto 8 ---------------------------------------------------------------