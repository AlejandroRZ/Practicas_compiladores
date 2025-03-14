module P2 where
import Data.Char(isAlpha,isDigit,isSpace)

-----------------------------------------------------------------------------------------
-- Miembros del equipo:
-- Gabriela Cruz Blanco
-- Javier Alejandro Rivera Zavala
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Sergio Vasconcelos Carranza


---------------------------------- Análisis léxico ----------------------------------

data Token = Var String | Number Int | Boolean Bool | Sum | Subs | And | Or | Equal 
            deriving Show
            
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexString (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = Sum : lexer cs
lexer ('-':cs) = Subs : lexer cs
lexer ('&':'&':rest) = And : lexer rest
lexer ('|':'|':rest) = Or : lexer rest
lexer ('=':'=':cs) = Equal : lexer cs

lexNum :: String -> [Token]
lexNum cs = Number (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexString :: String -> [Token]
lexString cs =
   case span isAlpha cs of
      ("t", rest) -> Boolean (True) : lexer rest
      ("f", rest) -> Boolean (False) : lexer rest
      (var,rest)   -> Var (var) : lexer rest

      
---------------------------------- Análisis sintáctico ----------------------------------

data ASA = VarASA String | NumberASA Int | BooleanASA Bool | Op Token ASA ASA deriving Show
type Stack = [ASA]

scanner :: [Token] -> ASA
scanner tokens = scannerAux tokens []

scannerAux :: [Token] -> Stack -> ASA
scannerAux [] (asa:_) = asa
scannerAux (token:tokens) pila
    | esOperando token = scannerAux tokens (convertirTokenASA token : pila)
    | esOperador token = case pila of
        (derecha : izquierda : pilaRestante) -> scannerAux tokens (Op token derecha izquierda : pilaRestante)
        _ -> error "Expresión mal formada"
    | otherwise = error "Token desconocido"

esOperando :: Token -> Bool
esOperando (Var _) = True
esOperando (Number _) = True
esOperando (Boolean _) = True
esOperando _ = False

esOperador :: Token -> Bool
esOperador Sum = True
esOperador Subs = True
esOperador And = True
esOperador Or = True
esOperador Equal = True
esOperador _ = False

convertirTokenASA :: Token -> ASA
convertirTokenASA (Var x) = VarASA x
convertirTokenASA (Number n) = NumberASA n
convertirTokenASA (Boolean b) = BooleanASA b
convertirTokenASA _ = error "Token desconocido"

      
---------------------------------- Análisis semántico ----------------------------------

data Type = Num | Bool deriving Show

-- Función TypeChecker recibe un ASA y devuelve dicho ASA si el tipado del programa es consistente.
typeChecker :: ASA -> ASA
typeChecker asa = typeCheckerAux asa `seq` asa

-- Función TypeCheckerAux recibe un ASA y devuelve el tipo de la expresión únicamente si el tipado
-- del programa es consistente. En otro caso arroja un error indicando el problema con el programa.
typeCheckerAux :: ASA -> Type
typeCheckerAux (NumberASA _) = Num
typeCheckerAux (VarASA _) = Num
typeCheckerAux (BooleanASA _) = Bool
typeCheckerAux (Op op a b) = 
    case op of
        Sum   -> case (ta, tb) of
                     (Num, Num) -> Num
                     _ -> error $ "El tipo de los argumentos " ++ show a ++ " y " ++ show b ++ " no son los esperados para el operador Sum"
        Subs  -> case (ta, tb) of
                     (Num, Num) -> Num
                     _ -> error $ "El tipo de los argumentos " ++ show a ++ " y " ++ show b ++ " no son los esperados para el operador Subs"
        And   -> case (ta, tb) of
                     (Bool, Bool) -> Bool
                     _ -> error $ "El tipo de los argumentos " ++ show a ++ " y " ++ show b ++ " no son los esperados para el operador And"
        Or    -> case (ta, tb) of
                     (Bool, Bool) -> Bool
                     _ -> error $ "El tipo de los argumentos " ++ show a ++ " y " ++ show b ++ " no son los esperados para el operador Or"
        Equal -> case (ta, tb) of
                     (Num, Num) -> Bool
                     _ -> error $ "El tipo de los argumentos " ++ show a ++ " y " ++ show b ++ " no son los esperados para el operador Equal"
    where
        ta = typeCheckerAux a
        tb = typeCheckerAux b

      
---------------------------------- Optimización de Código Fuente ----------------------------------

-- Función que que recibe un ASA y devuelve el ASA resultante de aplicarle plegado constante.
constantFolding :: ASA -> ASA
constantFolding (Op op left right) = foldOperation op (constantFolding left) (constantFolding right)
constantFolding asa = asa

-- Función que toma un token y dos ASA y realiza una operación definida, 
-- combinando estos elementos para producir un nuevo árbol sintáctico abstracto.
foldOperation :: Token -> ASA -> ASA -> ASA
foldOperation Sum (NumberASA x) (NumberASA y) = NumberASA (x + y)
foldOperation Subs (NumberASA x) (NumberASA y) = NumberASA (x - y)

foldOperation Equal (NumberASA x) (NumberASA y) 
    | x == y    = BooleanASA True
    | otherwise = BooleanASA False

foldOperation And (BooleanASA True) r = r
foldOperation And l (BooleanASA True) = l
foldOperation And (BooleanASA False) _ = BooleanASA False
foldOperation And _ (BooleanASA False) = BooleanASA False

foldOperation Or (BooleanASA True) _ = BooleanASA True
foldOperation Or _ (BooleanASA True) = BooleanASA True
foldOperation Or (BooleanASA False) r = r
foldOperation Or l (BooleanASA False) = l

foldOperation op left right = Op op left right


data Value = N Int | B Bool | S String
instance Show Value where
        show (N n) = show n
        show (B b) = show b
        show (S s) = show s

tokenThreeAddress :: Token -> String
tokenThreeAddress Sum = "+"
tokenThreeAddress Subs = "-"
tokenThreeAddress And = "&&"
tokenThreeAddress Or = "||"
tokenThreeAddress Equal = "=="

data ThreeAddress = Assign String Value | Operation String String Token String
instance Show ThreeAddress where
        show (Assign t v) = show t ++ " = " ++ show v
        show (Operation t a op b) = show t ++ " = " ++ show a ++ tokenThreeAddress op ++ show b

fresh :: [Int] -> Int
fresh ts = head [n | n <- [0..], n `notElem` ts]

threeAddressAux :: ASA -> [Int] -> ([ThreeAddress],String,[Int])
threeAddressAux (VarASA v) ts = (c',temp,i:ts)
                                    where 
                                        i = fresh ts
                                        temp = "t" ++ show i
                                        c' = [Assign temp (S v)]
threeAddressAux (NumberASA n) ts = (c',temp,i:ts)
                                    where 
                                        i = fresh ts
                                        temp = "t" ++ show i
                                        c' = [Assign temp (N n)]
threeAddressAux (BooleanASA b) ts = (c',temp,i:ts)
                                    where 
                                        i = fresh ts
                                        temp = "t" ++ show i
                                        c' = [Assign temp (B b)]
threeAddressAux (Op op a b) ts = (a' ++ b' ++ c',temp,i:bs)
                                    where 
                                        (a',adA,as) = threeAddressAux a ts
                                        (b',adB,bs) = threeAddressAux b as
                                        i = fresh bs
                                        temp = "t" ++ show i
                                        c' = [Operation temp adA op adB]

threeAddress :: ASA -> [ThreeAddress]
threeAddress asa = trad
                    where 
                    (trad,temp,ts) = threeAddressAux asa []


---------------------------------- Generación de Código ----------------------------------

--assembly :: [ ThreeAddress ] -> String
-- Función assembly
assembly :: [ThreeAddress] -> String
assembly prog = unlines $ map toAsm prog

-- Función toAssembly
toAsm :: ThreeAddress -> String
toAsm (Assign t v) = "MOV " ++ t ++ " " ++ valueToString v
toAsm (Operation t a op b) = operationToAsm op t a b

-- Función auxiliar para convertir valores en cadenas
valueToString :: Value -> String
valueToString (N n) = show n
valueToString (S s) = show s
valueToString (B b) = show b

-- Función auxiliar para traducir operaciones a lenguaje ensamblador
operationToAsm :: Token -> String -> String -> String -> String
operationToAsm Sum t a b = "ADD " ++ t ++ " " ++ a ++ " " ++ b
operationToAsm Subs t a b = "SUBS " ++ t ++ " " ++ a ++ " " ++ b
operationToAsm And t a b = "AND " ++ t ++ " " ++ a ++ " " ++ b
operationToAsm Or t a b = "OR " ++ t ++ " " ++ a ++ " " ++ b
operationToAsm Equal t a b = "EQ " ++ t ++ " " ++ a ++ " " ++ b

-- Para correr: primero ingresar la entrada y despues ponerlo con formato Así:
-- let entrada = [Assign "t0" (S "var"), Assign "t1" (N 25), Operation "t2" "t0" Equal "t1"]
-- putStr (assembly entrada)


----------------------------------  Compilación (extra) ---------------------------------- 

compile :: String -> String
compile prog = assembly $ threeAddress $ constantFolding $ typeChecker $ scanner $ lexer prog
