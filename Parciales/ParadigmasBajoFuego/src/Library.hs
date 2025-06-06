module Library where
import PdePreludat

fraseDeFreire = "El estudio no se mide por el número de páginas leídas en una noche, ni por la cantidad de libros leídos en un semestre. Estudiar no es un acto de consumir ideas, sino de crearlas y recrearlas"

--- Punto 1 : MODELAR DATOS
--- 1.a
data Catedra = CatedraActiva{
  nombreCatedra :: String,
  filosofia :: String,
  cantidadProfesores:: Number,
  cantidadAyudantes :: Number,
  temas :: [String]
} | CatedraInactiva {
  nombreCatedra :: String,
  filosofia ::  String,
  aniosInactiva:: Number
} deriving (Show)

data Facultad = Facultad
  { diasJornadaCompleta :: Number,
    diasMediaJornada :: Number,
    catedras :: [Catedra]
  }deriving (Show)

--- 1.b
paradigmasDeProgramacion = CatedraActiva "Paradigma de Programacion" fraseDeFreire 13 52 ["expresividad", "lógico", "objetos", "declaratividad"]
sintaxis = CatedraActiva "Sintaxis" "Frase de prueba para test" 13 52 ["expresividad", "lógico", "objetos", "declaratividad"]
discreta = CatedraInactiva "Discreta" "frase de matemática discreta" 1
utn = Facultad 7 0 [paradigmasDeProgramacion,sintaxis,discreta]
--- Punto 2
-- 2.a
ataquesBiologicos :: Number -> Catedra -> Catedra
ataquesBiologicos porcentaje catedra@(CatedraActiva {}) =
  catedra
    { cantidadProfesores = cantidadProfesores catedra - 2,
      cantidadAyudantes = cantidadAyudantes catedra - porcentajeAfectado
    }
  where
    porcentajeAfectado = (round . (/ 100) . (* porcentaje) . cantidadAyudantes) catedra
ataquesBiologicos _ catedra = catedra

-- 2.b
ataquesIdiologicos :: [Char] -> Catedra -> Catedra
ataquesIdiologicos letrasABorrar catedra@(CatedraActiva {}) = catedra {
  filosofia = quitarLetras (filosofia catedra) letrasABorrar,
  temas = (init . temas) catedra
}
ataquesIdiologicos _ catedra = catedra

quitarLetras :: Eq a => [a] -> [a] -> [a]
quitarLetras = foldl (\acc letra -> filter (/=letra) acc)

-- 2.c
refuerzosAyudantisticos :: Number -> (Catedra -> Catedra)
refuerzosAyudantisticos refuersos catedra@(CatedraActiva{ }) = catedra { cantidadAyudantes = cantidadAyudantes catedra + refuersos}
refuerzosAyudantisticos _ catedra = catedra

-- 2.d
resucitarCatedra :: Catedra -> (Catedra -> Catedra)
resucitarCatedra catedraActiva@(CatedraActiva {}) catedraInactiva@(CatedraInactiva {}) =
  catedraActiva {
    nombreCatedra = nombreCatedra catedraInactiva,
    filosofia = filosofia catedraInactiva
  }
resucitarCatedra catedra _ = catedra

-- 3
procesarEvento :: (Catedra -> Catedra) -> Catedra -> Catedra
procesarEvento evento catedra@(CatedraActiva {}) = evento catedra
procesarEvento _ catedra = catedra

-- 4
puedeDarClasesEnLaFacultad :: Catedra -> Facultad -> Bool
puedeDarClasesEnLaFacultad catedra facultad = puedeCubrirCursos && criterioFilosofia && criterioTemas && esCatedraActiva catedra
  where
    puedeCubrirCursos = (cantidadProfesores catedra * 2) >= (diasJornadaCompleta facultad *3 + diasMediaJornada facultad *2)
    criterioFilosofia = ((>30).length . filosofia) catedra&& ((>=10).length. words . filosofia) catedra
    criterioTemas = ((>=2).length.temas) catedra

esCatedraActiva :: Catedra -> Bool
esCatedraActiva catedra@(CatedraActiva{}) = True
esCatedraActiva _ = False

-- 5

-- test facultad evento = (flip . puedeDarClases facultad . procesarEvento evento)
test facultad catedra evento = (flip darDeBaja catedra . not . flip puedeDarClasesEnLaFacultad facultad.procesarEvento evento) catedra 
darDeBaja :: Bool -> Catedra -> Catedra
darDeBaja False catedra = catedra
darDeBaja True catedra = CatedraInactiva {
  nombreCatedra = nombreCatedra catedra,
  filosofia= filosofia catedra,
  aniosInactiva=0}
  
-- explicar el tipo que se infiere de la siguiente definicion de funcion.
