-----------------------------------------------------------------------------------------
-- Proyecto final - Compiladores
-- Miembros del equipo:
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Javier Alejandro Rivera Zavala
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- Versión con Parsec.
-----------------------------------------------------------------------------------------
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Error (newErrorMessage, Message (..), errorPos)

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

-----------------------------------------------------------------------------------------
-- Parsers para procesar cadenas en general.
-----------------------------------------------------------------------------------------

-- Parser para un carácter.
itemParser :: Parser Char
itemParser = anyChar

-- Parser para un string.
stringParser :: String -> Parser String
stringParser = try . string

-- Parser para espacios.
spaceParser :: Parser ()
spaceParser = skipMany (space <|> tab)

-- Parser para un token.
tokenParser :: String -> Parser String
tokenParser symb = stringParser symb <* spaceParser

-----------------------------------------------------------------------------------------
-- Parsers para procesar cadenas que definen expresiones aritméticas del lenguaje.
-----------------------------------------------------------------------------------------

-- Parser para un operador de multiplicación o división.
multParser :: Parser (Expr -> Expr -> Expr)
multParser = tokenParser "*" *> pure Producto <|> tokenParser "/" *> pure Division

-- Parser para un operador de suma o resta
sumParser :: Parser (Expr -> Expr -> Expr)
sumParser = tokenParser "+" *> pure Suma <|> tokenParser "-" *> pure Resta

-- Parser para un número entero.
enteroParser :: Parser Expr
enteroParser = (Negativo <$> (tokenParser "-" *> factor) <|> Entero . read <$> many1 digit) <* spaceParser

-- Parser para un factor.
factor :: Parser Expr
factor = entreParentesis <|> enteroParser

-- Parser para una expresión entre paréntesis.
entreParentesis :: Parser Expr
entreParentesis = ParenExpr <$> (tokenParser "(" *> expr <* tokenParser ")")

-- Parser para términos en expresiones aritméticas. 
term :: Parser Expr
term = try (sumParser <*> factor <*> factor) <|> factor

-- Parser para expresiones aritméticas.
expr :: Parser Expr
expr = try (multParser <*> factor <*> factor) <|> term

-----------------------------------------------------------------------------------------
-- Parser principal del lenguaje.
-----------------------------------------------------------------------------------------

-- Función principal para el análisis sintáctico de expresiones aritméticas.
parserAritmetico :: String -> Either ParseError Expr
parserAritmetico cs =
  case parse (spaceParser *> expr <* eof) "" cs of
    Left err -> Left (newErrorMessage (Message "Expresión no válida") (errorPos err))
    result   -> result

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