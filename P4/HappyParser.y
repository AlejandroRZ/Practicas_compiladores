-----------------------------------------------------------------------------------------
-- Miembros del equipo:
-- Gabriela Cruz Blanco
-- Javier Alejandro Rivera Zavala
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Sergio Vasconcelos Carranza
-----------------------------------------------------------------------------------------
{
module HappyParser where

import Data.Char
}

%name parser
%tokentype { Token }
%error { parseError }

%token 
      ":="            { Assign }
      if              { If }
      then            { Then }
      else            { Else }
      ';'             { Seq }
      while           { While }
      do              { Do }
      skip            { Skip }
      bool            { Boolean $$ }
      '='             { Equal }
      '&'             { And }
      '-'             { Not }
      L               { Loc $$ }
      num             { Number $$ }
      '('             { LP }
      ')'             { RP }
      '+'             { Sum }
      
%%

C : L ":=" E                          { AssignASA $1 $3 } 
  | if B then C else C                { IfThenElseASA $2 $4 $6 }
  | '(' C ';' C ')'                   { SeqASA $2 $4 }
  | while B do C                      { WhileDoASA $2 $4 }
  | skip                              { SkipASA }

B : bool                              { BoolASA $1 }
  | E '=' E                           { EqualASA $1 $3 }
  | '&' B B                           { AndASA $2 $3 }
  | '-' B                             { NotASA $2 }

E : L                                 { LocASA $1 }
  | num                               { NumberASA $1 }
  | '(' E '+' E ')'                   { SumASA $2 $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"


data C
      = AssignASA Int E
      | IfThenElseASA B C C
      | SeqASA C C
      | WhileDoASA B C
      | SkipASA
      deriving (Eq,Show)

data B
      = BoolASA Bool
      | EqualASA E E
      | AndASA B B
      | NotASA B
      deriving (Eq,Show)

data E
      = LocASA Int
      | NumberASA Int
      | SumASA E E
      deriving (Eq,Show)


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


mainInter = getContents >>= print . parser . lexer
}
