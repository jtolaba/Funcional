module Library where

import PdePreludat

type Ingrediente = String

-------------------------------------------------------- Punto 1 --------------------------------------------------------
--- a)
data Pizza = Pizza
  { ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
  }
  deriving (Show)

---- b)
grandeDeMuzza :: Pizza
grandeDeMuzza = Pizza ["salsa", "mozzarella", "oregano"] 4 350

pepperoni :: Pizza
pepperoni = Pizza ["salsa", "mozzarella", "pepperoni"] 4 400

napolitana :: Pizza
napolitana = Pizza ["salsa", "mozzarella", "tomate", "ajo"] 4 380

cuatroQuesos :: Pizza
cuatroQuesos = Pizza ["salsa", "mozzarella", "parmesano", "roquefort", "fontina"] 4 420

hawaiana :: Pizza
hawaiana = Pizza ["salsa", "mozzarella", "jamón", "ananá"] 4 390

fugazzeta :: Pizza
fugazzeta = Pizza ["mozzarella", "cebolla", "oregano"] 4 370

roquefort :: Pizza
roquefort = Pizza ["salsa", "mozzarella", "roquefort"] 4 410

vegetariana :: Pizza
vegetariana = Pizza ["salsa", "mozzarella", "pimientos", "berenjena", "aceitunas", "cebolla"] 4 380

marinera :: Pizza
marinera = Pizza ["salsa", "mozzarella", "anchoas", "ajo", "aceitunas"] 4 400

mexicana :: Pizza
mexicana = Pizza ["salsa", "mozzarella", "chorizo", "jalapeño", "tomate"] 4 410

carbonara :: Pizza
carbonara = Pizza ["salsa blanca", "mozzarella", "panceta", "huevo"] 10 420

anchoasBasica :: Pizza
anchoasBasica = Pizza ["anchoas", "salsa"] 8 270

-------------------------------------------------------- Punto 2 --------------------------------------------------------
---- a)
nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion pizza
  | elem "palmitos" . ingredientes $ pizza = 0
  | (< 500) . calorias $ pizza = calculoCalorias
  | otherwise = calculoCalorias / 2
  where
    calculoCalorias = length (ingredientes pizza) * 80

-------------------------------------------------------- Punto 3 --------------------------------------------------------
valorPizza :: Pizza -> Number
valorPizza pizza = (* 120) . (* tamanio pizza) . length . ingredientes $ pizza

-------------------------------------------------------- Punto 4 --------------------------------------------------------
---- a)
nuevoIngrediente :: Ingrediente -> Pizza -> Pizza
nuevoIngrediente ingrediente = agregarCalorias ((* 2) . length $ ingrediente) . agregarIngrediente ingrediente

agregarCalorias :: Number -> Pizza -> Pizza
agregarCalorias valor pizza = pizza {calorias = calorias pizza + valor}

agregarIngrediente :: String -> Pizza -> Pizza
agregarIngrediente ingrediente pizza = pizza {ingredientes = ingrediente : ingredientes pizza}

---- b)
agrandar :: Pizza -> Pizza
agrandar pizza = pizza {tamanio = min 10 (tamanio pizza + 2)}

---- c)
mezcladita :: Pizza -> Pizza -> Pizza
mezcladita pizza pizza2 = agregarCalorias (calorias pizza / 2) (agregarIngredientes pizza2 pizza)

ingredientesNoRepetidos :: Pizza -> Pizza -> [Ingrediente]
ingredientesNoRepetidos p1 p2 = filter (not . (`elem` ingredientes p1)) (ingredientes p2)

agregarIngredientes :: Pizza -> Pizza -> Pizza
agregarIngredientes p1 p2 = foldr agregarIngrediente p1 (ingredientesNoRepetidos p1 p2)

-------------------------------------------------------- Punto 5 --------------------------------------------------------
nivelDeSatisfaccionPedido :: [Pizza] -> Number
nivelDeSatisfaccionPedido = sum . map nivelDeSatisfaccion

-------------------------------------------------------- Punto 6 --------------------------------------------------------
---- a)
pizzeriaLosHijosDePato :: [Pizza] -> [Pizza]
pizzeriaLosHijosDePato = map (agregarIngrediente "palmito")

---- b)
pizzeriaElResumen :: [Pizza] -> [Pizza]
pizzeriaElResumen [] = []
pizzeriaElResumen [pizza] = [pizza]
pizzeriaElResumen pizzas = zipWith mezcladita pizzas (tail pizzas)

---- c)
pizzeriaEspecial :: Pizza -> [Pizza] -> [Pizza]
pizzeriaEspecial pizza = map (mezcladita pizza)

pizzeriaPescadito :: [Pizza] -> [Pizza]
pizzeriaPescadito = map (mezcladita anchoasBasica)

---- d)

pizzeriaGourmet :: Number -> [Pizza] -> [Pizza]
pizzeriaGourmet nivelExquisitez pizzas = map (agrandar . fst) (filter ((>= nivelExquisitez) . snd) (zip pizzas (map nivelDeSatisfaccion pizzas)))

pizzeriaLaJauja :: [Pizza] -> [Pizza]
pizzeriaLaJauja pizzas = map (agrandar . fst) (filter ((>= 399) . snd) (zip pizzas (map nivelDeSatisfaccion pizzas)))

-- Prueba de listas enlazada?
enlazandoListas a = zip a (tail a)

-------------------------------------------------------- Punto 7 --------------------------------------------------------

-------------------------------------------------------- Punto 8 --------------------------------------------------------
-- Explicar el tipo de la siguiente función:
yoPidoCualquierPizza :: (a -> Number) -> (b -> Bool) -> [(a, b)] -> Bool
yoPidoCualquierPizza x y z = any (odd . x . fst) z && all (y . snd) z