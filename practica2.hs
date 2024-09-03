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

sustituir :: String -> Prop -> Prop -> Prop
sustituir x sub (Var y) = if x == y then sub else Var y
sustituir _ _ (Cons b) = Cons b
sustituir x sub (Not p) = Not (sustituir x sub p)
sustituir x sub (And p q) = And (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Or p q) = Or (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Impl p q) = Impl (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Syss p q) = Syss (sustituir x sub p) (sustituir x sub q)
{-
estadosPosibles :: Prop -> [[a]]
estadosPosibles (Var v) xs = v `elem` xs 
estadosPosibles (Cons b) _ = b
estadosPosibles (Not f) xs = not (estadosPosibles f xs)
estadosPosibles (And f1 f2) xs = estadosPosibles f1 xs && estadosPosibles f2 xs
estadosPosibles (Or f1 f2) xs = estadosPosibles f1 xs || estadosPosibles f2 xs
estadosPosibles (Impl f1 f2) xs = estadosPosibles (Not f1) xs || estadosPosibles f2 xs
estadosPosibles (Syss f1 f2) xs = estadosPosibles (Impl f1 f2) xs && estadosPosibles (Impl f2 f1) xs
-}

contar :: Prop -> Int
contar (Var y) = 1
contar (Cons b) = 0
contar (Not p ) = (contar p)
contar (And p q) = (contar p) + (contar q)
contar (Or p q) = (contar p) +(contar q)
contar (Impl p q) = (contar p) +(contar q)
contar (Syss p q) = (contar p) +(contar q)

-- E5 Definir una función que dada una fórmula proposicional, nos diga si es una tautología.

-- E6 Definir una función que dada una fórmula proposicional, nos diga si es una contradicción.
--
-- E7 Definir una función que dada una interpretación y una fórmula proposicional, verifique si esta interpretación es un modelo.
--
-- E8 Definir una función que dada una fórmula proposicional, esta devuelve la lista de todos sus modelos.
--
-- E9 Definir una función que dada una fórmula proposicional, verifica si esta es válida o no.
--
-- E10 Definir una función que dada una fórmula proposicional, verifica si esta es insatisfacible.
--
-- E11 Definir una función que dada una fórmula proposicional, verifica si esta es satisfacible. 






