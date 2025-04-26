module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--- 1.a
data Pizza = Pizza
  { ingredientes :: [String],
    tamaño :: Number,
    calorias :: Number
  }
  deriving (Show)
--- 1.b
grandeDeMuzza = Pizza ["salsa","mozzarella","oregano"] 8 350

--- 2.a 
nivelDeSastifaccion :: Pizza -> Number
nivelDeSastifaccion pizza
  | elem "palmitos". ingredientes$pizza = 0
  | (<500).calorias$pizza = calculoCalorias
  | otherwise = calculoCalorias / 2
  where
  calculoCalorias = length(ingredientes pizza) * 80