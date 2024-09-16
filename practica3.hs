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

-- Fin de pre - código --


-- E1.1 Implementar la funcion fnn que convierte una fórmula proproposicional en su forma normal negativa.

fnn :: Prop -> Prop
fnn (Var p) = Not (Var p)
fnn (Cons True) = True
fnn (Cons False) = False
fnn (Not f) = f 
fnn (And f1 f2) = (Or (negar f1) (negar f2))
fnn (Or f1 f2) = (And (negar f1) (negar f2))
fnn (Impl f1 f2) = (And f1 (negar f2))
fnn (Syss f1 f2) = negar (And (Impl f1 f2) (Impl f2 f1))


-- E1.2 Impementar la función fnc, que convierte una fórmula proposicional en su forma normal conjuntiva. Se recomienda usar la función fnn.

fnc :: Prop -> Prop

-- E2.1 Crear un sinónimo Literal, que será igual a Prop por simplicidad, aunque solo deberían ser variables o negaciones de variables. 

type Literal = Prop

-- E2.2 Crear un sinónimo Clausula, que representará las claúsuas como conjunto de literales.

type Clausula = [Literal]

-- E2.3 Definir la función clausulas que dada una fórmula en FNC, devuelve una lista con cláusulas que la forman.

clausulas :: Prop -> [Clausula]

-- E2.4 Definir la función resolución que dadas dos cláusulas, devuelve el resolvente obtenido después de aplicar la regla de resolución binaria. Se puede asumir que se puede obtener un resolvente a partir de los argumentos.

resolucion :: Clausula -> Clausula -> Clausula

-- E3.1 Definir la función hayResolvente, que determina si es posible obtener un resolvente a partir de dos cláusulas.

hayResolvente :: Clausula -> Clausula -> Bool

-- E3.2 Definir la función saturacion, que dada una fórmula proposicional, determina si esta es satisfacible o no usando el algoritmo de saturación.

saturacion :: Prop -> Bool
