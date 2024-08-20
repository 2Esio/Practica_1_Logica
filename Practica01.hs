
-- 1. Tipos de datos Algebraicos

-- 1.1 Shape 

data Shape = Circle Float | Square Float | Rectangle Float Float
        | Triangle Float Float | Trapeze Float Float Float deriving (Show, Eq)

area :: Shape -> Float 
area (Circle r) = pi * r ** 2 
area (Square l) = l ** 2
area (Rectangle l w) = l * w
area (Triangle b h) = (b * h)/2
area (Trapeze a b h) = ((a + b)/2) * h

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = l * 4
perimeter (Rectangle l w) = 2 * (l + w)
perimeter (Triangle b h) = b * 3
perimeter (Trapeze a b h) = a + b + 2 * sqrt ((abs(a - b)/2)^2 + h^2)

instance Ord Shape where
    compare :: Shape -> Shape -> Ordering
    compare s1 s2 = compare (area s1) (area s2)

-- 1.2 Point

type Point = (Float, Float)

distance :: Point -> Point -> Float
distance (x_1, y_1) (x_2, y_2)= sqrt((x_2 - x_1)**2 +(y_2 - y_1)**2)

from0 :: Point -> Float 
from0 (x, y) = distance (0,0) (x,y) 



{- Ejercicio 4.4: 
Implementar usando foldr la función reversaFr, la cuál dada una lista nos regresa la lista con
los mismos elementos pero en orden opuesto.
-}

{- Función aux que usaremos en reversaFr con foldr
Recibe un elemento x y una lista xs,
regresa la lista con el elemento concatenado al final 
-}

agregarFinal :: a -> [a] -> [a]
agregarFinal x xs = xs ++ [x]

{- Función principal que usa foldr en la lista de entrada
(comenzará con la lista vacía) y se utiliza la concatenación.
El elemento x se coloca al final de la lista xs y se concatenan.
Foldr aplicara a cada elemento de la lista nuestra función auxiliar,
para así, construir la lista al revés-}

reversaFr :: [a] -> [a]
reversaFr = foldr agregarFinal []










