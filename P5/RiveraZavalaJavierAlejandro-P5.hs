-----------------------------------------------------------------------------------------
-- Miembros del equipo:
-- Javier Alejandro Rivera Zavala
-----------------------------------------------------------------------------------------

-- Definimos los Tokens que representan los diferentes elementos léxicos del lenguaje WHILE.
data Token = Assign | If | Then | Else | Seq | While | Do | Skip |
            Boolean Bool | Equal | And | Not |
            Loc Int | Number Int | LP | RP | Sum deriving Show
            
-- Definimos los tipos de datos y sinónimos de tipos pertinentes.
data Content = T Token | S | PC | C | PB | B | PE| E deriving Show
data State = Q Int deriving Show
type Input = [ Token ]
type Stack = [ State ]
type Symbols = [ Content ]

parser :: Input -> Bool
parser input = parserAux input [Q 0] []

parserAux:: Input -> Stack -> Symbols -> Bool
--SHIFT
--Verificamos el primer token del input
--Verificamos el estado en que nos encontramos
--Continuamos la derivacion transicionando al agregar un nuevo estado y el simbolo que leimos
parserAux ((Loc num):xs) (Q 0:ys) ws = parserAux xs (Q 3:Q 0:ys) (T (Loc num):ws)
parserAux (Skip:xs) (Q 0:ys) ws = parserAux xs (Q 4:Q 0:ys) (T Skip:ws)
parserAux (Seq :xs) (Q 2:ys) ws = parserAux xs (Q 5:Q 2:ys) (T Seq:ws)
parserAux (Assign:xs) (Q 3:ys) ws = parserAux xs (Q 6:Q 3:ys) (T Assign:ws)
parserAux ((Loc num):xs) (Q 5:ys) ws = parserAux xs (Q 3:Q 5:ys) (T (Loc num):ws)
parserAux (Skip:xs) (Q 5:ys) ws = parserAux xs (Q 4:Q 5:ys) (T Skip:ws)
parserAux ((Loc num):xs) (Q 6:ys) ws = parserAux xs (Q 10:Q 6:ys) (T (Loc num):ws)
parserAux ((Number n):xs) (Q 6:ys) ws = parserAux xs (Q 11:Q 6:ys) (T (Number n):ws)
parserAux (Sum:xs) (Q 9:ys) ws = parserAux xs (Q 12:Q 9:ys) (T Sum:ws)
parserAux ((Loc num):xs) (Q 12:ys) ws = parserAux xs (Q 10:Q 12:ys) (T (Loc num):ws)
parserAux ((Number n):xs) (Q 12:ys) ws = parserAux xs (Q 11:Q 12:ys) (T (Number n):ws)

--REDUCE 
--Verificamos el primer token del input (el simbolo de fin de cadena es equivalente a un input vacio)
--Verificamos el estado en el que nos encontramos
--Verificamos que los simbolos y estados que vamos a descartar son correctos
--Continuamos la derivacion reemplazando estados y simbolos
parserAux [] (Q 2:ys) (PC:ws) = parserAux [] (Q 1:ys) (C:ws)
parserAux (Seq:xs) (Q 4:ys) (T Skip:ws) = parserAux (Seq:xs) (Q 2:ys) (PC:ws)
parserAux [] (Q 4:ys) (T Skip:ws) = parserAux [] (Q 2:ys) (PC:ws)

parserAux [] (Q 7:Q 5:Q 2:ys) (C: T Seq: PC:ws) = parserAux [] (Q 1:ys) (C:ws)
parserAux (Seq:xs) (Q 8:Q 6:Q 3:ys) (E:T Assign:T (Loc num):ws) = parserAux (Seq:xs) (Q 2:ys) (PC:ws)
parserAux [] (Q 8:Q 6:Q 3:ys) (E:T Assign:T (Loc num):ws) = parserAux [] (Q 2:ys) (PC:ws)

parserAux (Seq:xs) (Q 9:ys) (PE:ws) = parserAux (Seq:xs) (Q 8:ys) (E:ws)
parserAux [] (Q 9:ys) (PE:ws) =parserAux [] (Q 8:ys) (E:ws)
  

parserAux (Seq:xs) (Q 10:ys) (T (Loc num):ws) = parserAux (Seq:xs) (Q 9:ys) (PE:ws)
parserAux (Sum:xs) (Q 10:ys) (T (Loc num):ws) = parserAux (Sum:xs) (Q 9:ys) (PE:ws)
parserAux [] (Q 10:ys) (T (Loc num):ws) = parserAux [] (Q 9:ys) (PE:ws)

parserAux (Seq:xs) (Q 11:ys) (T (Number n):ws) = parserAux (Seq:xs) (Q 9:ys) (PE:ws)
parserAux (Sum:xs) (Q 11:ys) (T (Number n):ws) = parserAux (Sum:xs) (Q 9:ys) (PE:ws)
parserAux [] (Q 11:ys) (T (Number n):ws) = parserAux [] (Q 9:ys) (PE:ws)

parserAux (Seq:xs) (Q 13:Q 12:Q 9:ys) (E:T Sum:PE:ws) = parserAux (Seq:xs) (Q 8:ys) (E:ws)
parserAux (Seq:xs) (Q 13:Q 12:Q 9:Q 12:ys) (E:T Sum:PE:ws) = parserAux (Seq:xs) (Q 13:Q 12:ys) (E:ws)
parserAux [] (Q 13:Q 12:Q 9:Q 6:ys) (E:T Sum:PE:ws) = parserAux [] (Q 8:Q 6:ys) (E:ws)
parserAux [] (Q 13:Q 12:Q 9:Q 12:ys) (E:T Sum:PE:ws) = parserAux [] (Q 13:Q 12:ys) (E:ws)

--ACCEPT
parserAux [] (Q 1:ys) ws = True

--REJECT
parserAux _ _ _ = False
