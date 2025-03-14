import Data.Char

-- Definimos los Tokens que representan los diferentes elementos léxicos del lenguaje WHILE.
data Token = Assign | If | Then | Else | Seq | While | Do | Skip |
            Boolean Bool | Equal | And | Not |
            Loc Int | Number Int | LP | RP | Sum deriving Show

-- La función principal lexer toma una cadena de entrada y devuelve una lista de tokens.
lexer :: String -> [Token]
lexer [] = []  
lexer input@(c:cs)
    | isSpace c = lexer cs    
    | isAlpha c = lexAlpha input  
    | isDigit c = lexNumber input  
    | otherwise = lexSymbol input  

-- La función lexAlpha procesa la parte alfabética de la cadena.
lexAlpha :: String -> [Token]
lexAlpha input =
    case span isAlphaNum input of
        (word, rest) ->
            case word of
                "if"    -> If : lexer rest
                "then"  -> Then : lexer rest
                "else"  -> Else : lexer rest
                "while" -> While : lexer rest
                "do"    -> Do : lexer rest
                "skip"  -> Skip : lexer rest
                "true"  -> Boolean True : lexer rest
                "false" -> Boolean False : lexer rest
                _       -> if isLocVar word
                               then Loc (read (drop 1 word)) : lexer rest
                               else error $ "Error: palabra no reconocida " ++ word
    where
        -- Verifica si la palabra es una variable Loc, si está formada por una "L" y un natural.
        isLocVar (l:ls) = l == 'L' && all isDigit ls
        isLocVar _      = False

-- La función lexNumber procesa la parte numérica de la cadena.
lexNumber :: String -> [Token]
lexNumber input =
    case span isDigit input of
        (num, rest) -> Number (read num) : lexer rest

-- La función lexSymbol procesa símbolos especiales.
lexSymbol :: String -> [Token]
lexSymbol (':':'=':cs) = Assign : lexer cs
lexSymbol (';':cs)     = Seq : lexer cs
lexSymbol ('=':cs)     = Equal : lexer cs
lexSymbol ('&':cs)     = And : lexer cs
lexSymbol ('-':cs)     = Not : lexer cs
lexSymbol ('(':cs)     = LP : lexer cs
lexSymbol (')':cs)     = RP : lexer cs
lexSymbol ('+':cs)     = Sum : lexer cs
lexSymbol s            = error $ "Error: símbolo no reconocido " ++ s