module Library where
import PdePreludat


-- Funcion dada en el enunciado.
cambiarElemento :: Number -> a -> [a] -> [a]
cambiarElemento posicion elemento lista = take (posicion - 1) lista ++ [ elemento ] ++ drop posicion lista

-- De cada edificio se conoce la composición de pisos que tiene, y de cada uno de estos, a su vez, los departamentos que lo componen. Además, de un edificio, se conoce el valor base por metro cuadrado y un coeficiente de robustez que va de 0 a 1. 
-- De los departamentos de cada piso se conoce su superficie en metros cuadrados y un porcentaje de habitabilidad, que va de 0 a 100.
-------------------------------------------------------- Punto 1 --------------------------------------------------------
data Edificio = Edificio {
  pisos :: [Piso],
  valorSuperficie :: Number,
  coeficienteRobustez :: Number
}deriving (Show)

data Piso = Piso{
  departamentos :: [Departamento]
}deriving (Show)

data Departamento = Departamento{
  superficie :: Number,
  habilitalidad :: Number
}deriving (Show)

-------------------------------------------------------- Punto 2 --------------------------------------------------------
-- calificarDepartamento: 
calificarDepartamento :: (Number -> Bool) -> Edificio -> Bool
calificarDepartamento condicion edificio = all (condicion . cantidadDepartamentos) (pisos edificio)

-- a. Cheto: decimos que un edificio es cheto, cuando todos sus pisos tienen un único departamento.
cheto :: Edificio -> Bool
cheto = calificarDepartamento (==1)

-- b. Pajarera: Cuando los pisos tienen al menos 6 departamentos cada uno.
pajarera :: Edificio -> Bool
pajarera = calificarDepartamento (>=6)


piramide :: Edificio -> Bool
piramide edificio = all (\(a,b) ->  cantidadDepartamentos a > cantidadDepartamentos b) paresDePisos
  where paresDePisos = zip (pisos edificio) (tail $ pisos edificio)

cantidadDepartamentos :: Piso -> Number
cantidadDepartamentos = length . departamentos

-------------------------------------------------------- Punto 3 --------------------------------------------------------
-- Conocer el precio del departamento más caro de un edificio, según su superficie y el valor base del metro cuadrado del edificio, multiplicado por el coeficiente de robustez del mismo.
departamentoMasCaro :: Edificio -> Number
departamentoMasCaro edificio =
    precio edificio . foldl1 (mayorPrecio edificio) . concatMap departamentos . pisos $ edificio

mayorPrecio :: Edificio -> Departamento -> Departamento -> Departamento
mayorPrecio edificio departamentoA departamentoB
  | precio edificio departamentoA > precio edificio departamentoB = departamentoA
  | otherwise                              
                         = departamentoB
precio :: Edificio -> Departamento -> Number
precio edificio = ( valorSuperficie edificio *) . (coeficienteRobustez edificio *). superficie


-------------------------------------------------------- Punto 4 --------------------------------------------------------
merge :: [Departamento] -> Departamento
merge departamentos = Departamento {
  superficie = sum (map superficie departamentos),
  habilitalidad = sum (map habilitalidad departamentos) / length departamentos
}

split :: Number -> Departamento -> [Departamento]
split cantidad departamento = replicate cantidad departamento {superficie = superficie departamento/ cantidad}
deptoPrueba = Departamento 80 20
edificioTest = Edificio [
  Piso [deptoPrueba,deptoPrueba,deptoPrueba,deptoPrueba],
  Piso [deptoPrueba,deptoPrueba,deptoPrueba],
  Piso [deptoPrueba,deptoPrueba],
  Piso [deptoPrueba]] 2200 0.89

-------------------------------------------------------- Punto 5 --------------------------------------------------------

terremoto :: Edificio -> Number -> Edificio
terremoto edificio cantidadAReducir = edificio {
  coeficienteRobustez = max 0 (coeficienteRobustez edificio - cantidadAReducir)
}

funcionLoca a b c = all ((>c) . fst a) . foldl (\x y -> b y . snd a $ x) []