-----------------------------------------------------------------------------------------
-- Proyecto final - Compiladores
-- Miembros del equipo:
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Javier Alejandro Rivera Zavala
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- Versión con STATE.
-----------------------------------------------------------------------------------------
import Control.Monad.State
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

-- Definición del parser usando el monad transformer StateT.
type ParserState a = StateT String [] a

-----------------------------------------------------------------------------------------
-- Parsers para procesar cadenas en general.
-----------------------------------------------------------------------------------------

-- Parser para un solo elemento (carácter).
itemParser :: ParserState Char
itemParser = StateT (\cs -> case cs of
  "" -> []
  (c:cs') -> [(c, cs')])

-- Parser para satisfacer un predicado en un carácter.
satisfy :: (Char -> Bool) -> ParserState Char
satisfy pred = itemParser >>= (\c -> if pred c then pure c else empty)

-- Parser para un carácter específico.
charParser :: Char -> ParserState Char
charParser c = satisfy (== c)

-- Parser para espacios.
spaceParser :: ParserState String
spaceParser = many (satisfy isSpace)

-- Parser para una cadena específica.
stringParser :: String -> ParserState String
stringParser "" = return ""
stringParser (c:cs) = (:) <$> charParser c <*> stringParser cs

-- Parser para un token específico con espacios opcionales.
tokenParser :: String -> ParserState String
tokenParser symb = spaceParser *> stringParser symb

-----------------------------------------------------------------------------------------
-- Parsers para procesar cadenas que definen expresiones aritméticas del lenguaje.
-----------------------------------------------------------------------------------------

-- Parser para operadores de multiplicación y división.
multParser :: ParserState (Expr -> Expr -> Expr)
multParser = tokenParser "*" *> pure Producto <|> tokenParser "/" *> pure Division

-- Parser para operadores de suma y resta.
sumParser :: ParserState (Expr -> Expr -> Expr)
sumParser = tokenParser "+" *> pure Suma <|> tokenParser "-" *> pure Resta

-- Parser para signo negativo.
negativoParser :: ParserState Expr
negativoParser = Negativo <$> (tokenParser "-" *> factor)

-- Parser para valores enteros.
enteroParser :: ParserState Expr
enteroParser = let positive = fmap (Entero . read) (some (satisfy isDigit))
               in spaceParser *> (negativoParser <|> positive)

-- Parser para factores, incluyendo enteros y expresiones dentro de paréntesis.
factor :: ParserState Expr
factor = enteroParser <|> entreParentesis

-- Parser para términos en expresiones aritméticas.
term :: ParserState Expr
term = (sumParser <*> factor <*> factor) <|> factor

-- Parser para expresiones aritméticas.
expr :: ParserState Expr
expr = (multParser <*> factor <*> factor) <|> term

-- Parser para expresiones dentro de paréntesis.
entreParentesis :: ParserState Expr
entreParentesis = ParenExpr <$> (tokenParser "(" *> expr <* tokenParser ")")

-----------------------------------------------------------------------------------------
-- Parser principal del lenguaje.
-----------------------------------------------------------------------------------------

-- Función principal para el análisis sintáctico de expresiones aritméticas.
parserAritmetico :: String -> [Expr]
parserAritmetico cs =
  case results of
    [] -> error "Expresión no válida"    
    _ -> results
  where
    results = evalStateT expr cs    

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