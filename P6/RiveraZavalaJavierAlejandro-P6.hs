-----------------------------------------------------------------------------------------
-- Miembros del equipo:
-- Gabriela Cruz Blanco
-- Javier Alejandro Rivera Zavala
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Sergio Vasconcelos Carranza
-----------------------------------------------------------------------------------------

-- Definiciones de datos
data ASA = Assign ASA ASA | IfThenElse ASA ASA ASA | Seq ASA ASA | WhileDo ASA ASA | Skip 
         | Boolean Bool | Equal ASA ASA | And ASA ASA | Not ASA
         | Loc Int | Number Int | Sum ASA ASA deriving Show

data Type = Num | Bool | Void deriving Show


typeChecker :: ASA -> ASA
typeChecker asa = if isVoid (typeCheckerAux asa)
                  then asa
                  else error "El tipado del programa no es consistente o el programa no es admisible"


typeCheckerAux :: ASA -> Type
typeCheckerAux (Loc _) = Num
typeCheckerAux (Number _) = Num
typeCheckerAux (Boolean _) = Bool
typeCheckerAux Skip = Void
typeCheckerAux (Sum x y) = checkBinaryTypes x y Num
typeCheckerAux (And p q) = checkBinaryTypes p q Bool
typeCheckerAux (Not p) = checkUnaryType p Bool
typeCheckerAux (Equal p q) = checkEqualTypes p q Num
typeCheckerAux (Assign x y) = checkAssignTypes x y
typeCheckerAux (IfThenElse p q r) = checkIfThenElseTypes p q r
typeCheckerAux (Seq p q) = checkSeqTypes p q
typeCheckerAux (WhileDo p q) = checkWhileTypes p q


checkBinaryTypes :: ASA -> ASA -> Type -> Type
checkBinaryTypes x y expectedType
  | not (compareTypes (typeCheckerAux x) expectedType) = error $ "El tipo de " ++ show x ++ " no es el esperado"
  | not (compareTypes (typeCheckerAux y) expectedType) = error $ "El tipo de " ++ show y ++ " no es el esperado"
  | otherwise = expectedType


checkUnaryType :: ASA -> Type -> Type
checkUnaryType x expectedType =
    if not (compareTypes (typeCheckerAux x) expectedType)
    then error $ "El tipo de " ++ show x ++ " no es el esperado"
    else expectedType


checkEqualTypes :: ASA -> ASA -> Type -> Type
checkEqualTypes x y expectedType
  | not (compareTypes (typeCheckerAux x) expectedType) = error $ "El tipo de " ++ show x ++ " no es el esperado"
  | not (compareTypes (typeCheckerAux y) expectedType) = error $ "El tipo de " ++ show y ++ " no es el esperado"
  | otherwise = Bool


--checkAssignTypes :: ASA -> ASA -> Type
--checkAssignTypes x y  
  -- | not (compareTypes (typeCheckerAux y) Num) = error $ "El tipo de " ++ show y ++ " no es el esperado"
  -- | otherwise = Void

checkAssignTypes :: ASA -> ASA -> Type
checkAssignTypes (Loc _) y
  | not (compareTypes (typeCheckerAux y) Num) = error $ "El tipo de " ++ show y ++ " no es el esperado"
  | otherwise = Void
checkAssignTypes x _ = error $ "Se esperaba una variable de tipo Loc, pero se encontró " ++ show x



checkIfThenElseTypes :: ASA -> ASA -> ASA -> Type
checkIfThenElseTypes p q r
  | not (compareTypes (typeCheckerAux p) Bool) = error $ "El tipo de " ++ show p ++ " no es el esperado"
  | not (isVoid (typeCheckerAux q)) || not (isVoid (typeCheckerAux r)) = error $ "El tipo de los argumentos " ++ show q ++ " y " ++ show r ++ " no son los esperados"
  | otherwise = Void

checkSeqTypes :: ASA -> ASA -> Type
checkSeqTypes p q =
  if not (isVoid (typeCheckerAux p)) || not (isVoid (typeCheckerAux q))
  then error $ "El tipo de los argumentos " ++ show p ++ " y " ++ show q ++ " no son los esperados"
  else Void


checkWhileTypes :: ASA -> ASA -> Type
checkWhileTypes p q
  | not (compareTypes (typeCheckerAux p) Bool) = error $ "El tipo de " ++ show p ++ " no es el esperado"
  | not (isVoid (typeCheckerAux q)) = error $ "El tipo de " ++ show q ++ " no es el esperado"
  | otherwise = Void


compareTypes :: Type -> Type -> Bool
compareTypes Num Num = True
compareTypes Bool Bool = True
compareTypes Void Void = True
compareTypes _ _ = False

isVoid :: Type -> Bool
isVoid Void = True
isVoid _ = False
