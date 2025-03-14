-- Declaración del módulo WhileLexer
{
module WhileLexer where
}
-- Importa el generador básico de analizadores léxicos de Alex
%wrapper "basic"

-- Define un conjunto de reglas para reconocer los tokens
$digit = 0-9

-- Define los tokens 
tokens :-

    $white+      ;
    "--".*       ;
    
    "if"         { \s -> If }
    "then"       { \s -> Then }
    "else"       { \s -> Else }
    "while"      { \s -> While }
    "do"         { \s -> Do }
    "skip"       { \s -> Skip }
    "true"       { \s -> Boolean True }
    "false"      { \s -> Boolean False }
    ":="         { \s -> Assign }
    ";"          { \s -> Seq }
    "="          { \s -> Equal }
    "&"          { \s -> And }
    "-"          { \s -> Not }
    "("          { \s -> LP }
    ")"          { \s -> RP }
    "+"          { \s -> Sum }
    "L"$digit+   { \s -> Loc (read (tail s)) }
    $digit+      { \s -> Number (read s) }

-- Sección de código Haskell
{

-- Definición de los tipos de tokens

data Token = Assign | If | Then | Else | Seq | While | Do | Skip |
             Boolean Bool | Equal | And | Not |
             Loc Int | Number Int | LP | RP | Sum deriving Show

-- Función lexer que utiliza alexScanTokens para convertir una cadena en una lista de tokens
lexer :: String -> [Token]
lexer = alexScanTokens

}
