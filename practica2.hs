-- Pre - código --
data Prop = Var String | Cons Bool | Not Prop
 | And Prop Prop | Or Prop Prop
 | Impl Prop Prop | Syss Prop Prop deriving (Eq)


instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

-- Termina pre - código --
--
-- Inicia funciones --  

-- E1 Definir la función variables :: Prop -> [String] tal que variables f devuelve el conjunto formado por todas las variables proposicionales que aparecen en f. 

eliminarRepetidos :: (Eq a)=> [a]-> [a]
eliminarRepetidos [ ] = [ ]
eliminarRepetidos (x:xs) = if x `elem` xs then eliminarRepetidos xs else [x] ++ eliminarRepetidos xs

variables :: Prop -> [String]
variables (Var y) = [y]
variables (Cons b) = []
variables (Not p ) =  eliminarRepetidos(variables p)
variables (And p q) = eliminarRepetidos(variables p ++ variables q)
variables (Or p q) = eliminarRepetidos(variables p ++ variables q)
variables (Impl p q) = eliminarRepetidos(variables p ++ variables q)
variables (Syss p q) = eliminarRepetidos(variables p ++ variables q)

-- E2 Definir la función conjPotencia :: [a] -> [[a]] tal que conjPotencia x devuelve la lista de todos los subconjuntos de x.

conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

-- E3 Definir la función interpretacion :: Prop -> Estado -> Bool tal que interpretacion f i es la interpretacion de f bajo i.

interpretacion :: Prop -> Estado -> Bool
interpretacion (Var v) xs = v `elem` xs 
interpretacion (Cons b) _ = b
interpretacion (Not f) xs = not (interpretacion f xs)
interpretacion (And f1 f2) xs = interpretacion f1 xs && interpretacion f2 xs
interpretacion (Or f1 f2) xs = interpretacion f1 xs || interpretacion f2 xs
interpretacion (Impl f1 f2) xs = interpretacion (Not f1) xs || interpretacion f2 xs
interpretacion (Syss f1 f2) xs = interpretacion (Impl f1 f2) xs && interpretacion (Impl f2 f1) xs

-- E4 Definir una función que dada una fórmula proposicional, la función devuelve los estados con los que podemos evaluar la fórmula.
estadosPosibles :: Prop -> [Estado]
estadosPosibles f = [estado | estado <- conjPotencia (variables f), interpretacion f estado]

f = Or (Var "q") (And (Var "r") (Var "q"))

-- E5 Definir una función que dada una fórmula proposicional, nos diga si es una tautología.

tautologia :: Prop -> Bool
tautologia f = estadosPosibles f == conjPotencia (variables f)

-- E6 Definir una función que dada una fórmula proposicional, nos diga si es una contradicción.

contradiccion :: Prop -> Bool
contradiccion f = estadosPosibles f == []

-- E7 Definir una función que dada una interpretación y una fórmula proposicional, verifique si esta interpretación es un modelo.

esModelo :: Estado -> Prop -> Bool
esModelo i f = interpretacion f i 

-- E8 Definir una función que dada una fórmula proposicional, esta devuelve la lista de todos sus modelos.
modelos :: Prop -> [Estado]
modelos f = estadosPosibles f

-- E9 Definir una función que dada una fórmula proposicional, verifica si esta es válida o no.

esValida :: Prop -> Bool
esValida = tautologia

-- E10 Definir una función que dada una fórmula proposicional, verifica si esta es insatisfacible.
--

esInsatisfacible :: Prop -> Bool
esInsatisfacible f = null(estadosPosibles f) 

-- E11 Definir una función que dada una fórmula proposicional, verifica si esta es satisfacible. 

esSatisfacible :: Prop -> Bool
esSatisfacible f = not(esInsatisfacible f)






