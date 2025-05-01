# 🍕 Parcial Funcional 2022 - Pizza Conmigo

> “La pizza no se discute, se modela (en Haskell).” – Alguien con hambre.

Este proyecto consiste en resolver el **Parcial de Paradigmas de Programación Funcional (04/06/2022)**, con el divertido y sabroso contexto de una cadena de pizzerías. Se trabaja con modelado de datos, funciones de orden superior, composición y aplicación parcial (¡evitando recursividad, salvo cuando se dice lo contrario!).

## 📦 Contenido

- Modelado de pizzas (ingredientes, tamaño, calorías)
- Funciones para manipular y analizar pizzas
- Modelado de pedidos (listas de pizzas)
- Comportamiento de diferentes pizzerías
- Análisis de satisfacción de pedidos
- Elección de la mejor pizzería
- Bonus: Composición de la pizzería perfecta

---

## 🧠 Conceptos Evaluados

- Uso extensivo de funciones de orden superior
- Composición y aplicación parcial
- Evitar recursividad innecesaria
- Reutilización de lógica
- Modelado con tipos algebraicos

---

## 📘 Ejercicios

Pizza, comida tan noble si la hay, con sus distintas variedades, que suelen generar amigos, rivales y hasta enemigos acérrimos.

La pizza tiene ingredientes, tamaño, y la cantidad de calorías.

El tamaño es una cantidad de porciones que va creciendo de a 2, a partir de 4. A modo de facilitar la lectura tenemos la siguiente escala:
- 4 porciones = individual,
- 6 = chica
- 8 = grande
- 10 = gigante


### 1. 📦 Modelado
- Generar un data modelando la pizza.
- Crear la función constante `grandeDeMuzza`, que es una pizza que tiene “salsa”, “mozzarella” y “orégano”, tiene 8 porciones, y tiene 350 calorías.

### 2. 😋 Nivel de satisfacción
- Si tiene palmito: 0.
- Cantidad de ingredientes * 80, siempre y cuando tenga menos de 500 calorías, en caso contrario es la mitad del cálculo.

### 3. 💰 Valor de una pizza
- Calcular el valor de una pizza, que es 120 veces la cantidad de ingredientes, multiplicado por su tamaño.

### 4. 🛠️ Transformaciones
- `nuevoIngrediente`: agrega ingrediente y suma calorías según la longitud del nombre.
- `agrandar`: suma 2 porciones (máximo 10).
- `mezcladita`: combina dos pizzas (únicos ingredientes, suma 50% de las calorías de la primera).

### 5. 😁 Satisfacción de un pedido
- Calcular el nivel de satisfacción de un pedido, que es la sumatoria de la satisfacción que brinda cada pizza que compone el mismo.

***Nota***: Usar composición

### 6. 🏪 Modelado de pizzerías

Cada pizzería es un mundo y, cuando hacemos un pedido y dependiendo de lo que queramos en el momento, optamos por una pizzería sobre otra. Aquí, vamos a modelar las pizzerías que conocemos:


- **`pizzeriaLosHijosDePato`**: A cada pizza del pedido le agrega palmito. ¿Por qué?... No hay “por qué”... Sólo que son unos verdaderos hijos de Pato.

- **`pizzeriaElResumen`**: Dado un pedido, entrega las combinaciones de una pizza con la siguiente. Es decir, la primera con la segunda, la segunda con la tercera, etc. (y, por lo tanto, termina enviando un pedido que tiene una pizza menos que el pedido original, por el resultado de la combinación de pares de pizzas). Si el pedido tiene una sola pizza, no produce cambios.

  ***Nota***: En esta definición puede usarse recursividad, aunque no es necesario. Pro-tip: función zip o zipWith.
- **`pizzeriaEspecial`**: Una pizzería especial tiene un sabor predilecto de pizza y todas las pizzas del pedido las combina con esa.
  - **`pizzeriaPescadito`** es un caso particular de este, donde su sabor predilecto es anchoas básica: tiene salsa, anchoas, sólo posee 270 calorías y es de 8 porciones.

- **`pizzeriaGourmet`**: Del pedido solo envía aquellas para las cuales el nivel de satisfacción supera el nivel de exquisitez de la pizzería... el resto no, las considera deplorables. Y, de regalo, a aquellas que manda las agranda a la siguiente escala, si esto es posible.
  - **`pizzeriaLaJauja`** es un clásico caso gourmet con un parámetro de exquisitez de 399.

### 7. 🏆 Pizzerías & Pedidos

- Implementar la función `sonDignasDeCalleCorrientes` que, dado un pedido y una lista de pizzerías, devuelve aquellas pizzerías que mejoran la satisfacción del pedido.
- Dado un pedido y una lista de pizzerías encontrar la pizzería que maximiza la satisfacción que otorga el pedido.

### 8. 🧮 Explicar el tipo de la siguiente función:
yoPidoCualquierPizza x y z = any (odd . x . fst) z && all (y . snd) z