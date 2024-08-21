
module Logica where


--Ejercicio 1
data Shape = Circle Float --radio
             | Square Float --lado
             | Rectangle Float Float --ladosverticales, ladoshorizontales
             | Triangle Float Float Float --lado1,lado2, lado3
             | Trapeze Float Float Float Float Float deriving (Show, Eq)--basemayor, basemenor, altura, lado1, lado2
--Funcion para calcular el area de las instancias de Shape
area :: Shape -> Float
area (Circle r) = pi * (r ** 2)
area (Square l) = l * l
area (Rectangle v h) = v * h

--https://www.geogebra.org/m/K42Xma77 Formula de Herón
area (Triangle a b c) =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

area (Trapeze bm m h l1 l2) = ((bm + m)/2) * h


--Funcion para calcular el perimetro de las instancias de Shape

perimeter :: Shape -> Float 
perimeter (Circle r) = pi * (r * 2)
perimeter (Square l) = l * 4
perimeter (Rectangle v h) = (v * 2) + (h * 2)
perimeter (Triangle a  b c) = a + b + c
perimeter (Trapeze bm m h l1 l2) = bm + m +l1 +l2

instance Ord Shape where
    compare :: Shape -> Shape -> Ordering
    compare s1 s2 = compare (area s1) (area s2)


--Ejercicio 2
data Point = Point Float Float deriving (Show)
--https://www.tusclasesparticulares.com/blog/como-calcular-distancia-entre-dos-puntos formula para calcular la distancia
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
--https://es.quora.com/C%C3%B3mo-calcular-la-distancia-entre-el-origen-y-el-punto distancia de un punto al origen
from0 :: Point -> Float
from0 (Point x y) = sqrt (x^2 + y^2)
--Ejercicio 3
data Haskellium = Haskellium{
    name :: String,
    lastName1 :: String,
    lastName2 :: String,
    location :: Point,
    houseShape :: Shape
} deriving (Show)
{-Dados dos Haskelliums y un String de nombre, regresa un Haskellium que sería hijo de
los dos Haskelliums con el nombre dado-}
son :: Haskellium -> Haskellium -> String -> Haskellium
son primerPadre segundoPadre nombre = Haskellium {
    name = nombre,
    lastName1 = lastName1 primerPadre,
    lastName2 = lastName2 segundoPadre,
    location = location primerPadre,
    houseShape = houseShape primerPadre
}
--Para calcular el costo de la casa de un Haskellium es necesario calcular el techo y las paredes 
--que tienen 2.5 de altura, el techo es el area de la figura que tiene la casa, y las paredes el perimetro por la altura

houseCost :: Haskellium -> Float
houseCost haskellium = (perimeter (houseShape haskellium) * 2.5) + (area (houseShape haskellium))

{-Haskellium, se calcula el tiempo en unidades t, que le cuesta llegar a su trabajo.
Contemplando si este va en bicicleta o en moto.
Notas: la plaza esta en el punto 0.0 por lo que solo es necesario usar from0 para determinar la distancia
entre la casa del haskellium y la plaza, la distancia maxima a recorrer en bici es de 300, y la bici va a 30
mientras que la moto se mueve a 70 -}

timeToWork :: Haskellium -> Float
timeToWork haskellium = if(distancia) <= 300 then distancia/30 else distancia/70
    where distancia = from0 (location haskellium)
