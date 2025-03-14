-----------------------------------------------------------------------------------------
-- Proyecto final - Compiladores
-- Miembros del equipo:
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Javier Alejandro Rivera Zavala
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- Versión que instancia directamente a Functor, Applicative, Monad y Alternative.
-----------------------------------------------------------------------------------------
import Data.Char (isDigit, isSpace)
import Control.Applicative

-----------------------------------------------------------------------------------------
-- Definición de tipos.
-----------------------------------------------------------------------------------------
data Expr
  = Suma Expr Expr
  | Resta Expr Expr
  | Producto Expr Expr
  | Division Expr Expr  
  | Negativo Expr
  | ParenExpr Expr
  | Entero Int  
  deriving Show

-- Nuevo tipo para el parser, que encapsula una función de parseo.
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-----------------------------------------------------------------------------------------
-- Parsers para procesar cadenas en general e instancias de Functor, Applicative, etc.
-----------------------------------------------------------------------------------------

-- Parser para un solo elemento (carácter).
itemParser :: Parser Char
itemParser = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c, cs)])

-- Parser para cersiorarse del complimiento de un predicado en un carácter.
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = itemParser >>= (\c -> if pred c then pure c else empty)

-- Parser para un carácter específico.
charParser :: Char -> Parser Char
charParser c = satisfy (== c)

-- Instancia de Functor para el tipo Parser.
instance Functor Parser where
  fmap f (Parser p) = Parser (\cs ->
    map (\(x, cs') -> (f x, cs')) (p cs))

-- Instancia de Applicative para el tipo Parser.
instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)])
  f <*> a = Parser (\cs ->
    concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs])

-- Instancia de Monad para el tipo Parser.
instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs ->
    concat [parse (f a) cs' | (a, cs') <- parse p cs])

-- Instancia de Alternative para el tipo Parser.
instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if length p' > 0 then p' else q')

-- Parser para espacios.
spaceParser :: Parser String
spaceParser = many (satisfy isSpace)

-- Parser para una cadena específica con espacios opcionales.
stringParser :: String -> Parser String
stringParser "" = return ""
stringParser (c:cs) = (:) <$> charParser c <*> stringParser cs

-- Parser para un token específico con espacios opcionales.
tokenParser :: String -> Parser String
tokenParser symb = spaceParser *> stringParser symb

-----------------------------------------------------------------------------------------
-- Parsers para procesar cadenas que definen expresiones aritméticas del lenguaje.
-----------------------------------------------------------------------------------------

-- Parser para operadores de multiplicación y división.
multParser :: Parser (Expr -> Expr -> Expr)
multParser = tokenParser "*" *> pure Producto <|> tokenParser "/" *> pure Division

-- Parser para operadores de suma y resta.
sumParser :: Parser (Expr -> Expr -> Expr)
sumParser = tokenParser "+" *> pure Suma <|> tokenParser "-" *> pure Resta

-- Parser para signo negativo.
negativoParser :: Parser Expr
negativoParser = Negativo <$> (tokenParser "-" *> factor)

-- Parser para valores enteros.
enteroParser :: Parser Expr
enteroParser = let positive = fmap (Entero . read) (some (satisfy isDigit))
               in spaceParser *> (negativoParser <|> positive)

-- Parser para factores, incluyendo enteros y expresiones dentro de paréntesis.
factor :: Parser Expr
factor = enteroParser <|> entreParentesis

-- Parser para términos en expresiones aritméticas.
term :: Parser Expr
term = (sumParser <*> factor <*> factor) <|> factor

-- Parser para expresiones aritméticas.
expr :: Parser Expr
expr = (multParser <*> factor <*> factor) <|> term

-- Parser auxiliar para poder controlar expresiones con y sin paréntesis.
parserAritAux :: Parser Expr
parserAritAux = expr

-- Parser para expresiones dentro de paréntesis.
entreParentesis :: Parser Expr
entreParentesis = ParenExpr <$> (tokenParser "(" *> parserAritAux <* tokenParser ")")

-----------------------------------------------------------------------------------------
-- Parser principal del lenguaje.
-----------------------------------------------------------------------------------------

-- Función para construir el ASA
buildASA :: [(Expr, String)] -> [Expr]
buildASA results = map fst results

-- Función principal para el análisis sintáctico de expresiones aritméticas.
parserAritmetico :: String -> [Expr]
parserAritmetico cs =
  case results of
    [(ast, "")] -> [ast]
    _ -> error "Expresión no válida"
  where
    results = parse parserAritAux cs

-----------------------------------------------------------------------------------------
-- Casos de prueba.
-----------------------------------------------------------------------------------------

-- Casos que debe de aceptar.

pruebaAcept1 :: String 
pruebaAcept1 = "((5))"

pruebaAcept2 :: String 
pruebaAcept2 = "/ -3 (* 5 4)"

pruebaAcept3 :: String 
pruebaAcept3 = "- - -5"

pruebaAcept4 :: String 
pruebaAcept4 = "+ (* -5 -3) (/ 4 2)"

pruebaAcept5 :: String 
pruebaAcept5 = "(+ 1 2)"

-- Casos que debe de rechazar.

pruebaRech1 :: String 
pruebaRech1 = "()"

pruebaRech2 :: String 
pruebaRech2 = "/ * -3 4 (* 5 4)"

pruebaRech3 :: String 
pruebaRech3 = "5 + 1"

pruebaRech4 :: String 
pruebaRech4 = "+ (- -1 -3 (* 3 8)"

pruebaRech5 :: String 
pruebaRech5 = "(+ 1 2) (* 4 5)"