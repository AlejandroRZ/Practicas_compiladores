module Practica01 where
import Data.List (sort)
import Data.Map (empty, insertWith, elems)

-----------------------------------------------------------------------------------------
-- Miembros del equipo:
-- Gabriela Cruz Blanco
-- Javier Alejandro Rivera Zavala
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Sergio Vasconcelos Carranza


-----------------------------------------------------------------------------------------
-- 1. La función groupAnagrams recibe una lista de String y devuelve una lista con los 
--    anagramas agrupados. 
groupAnagrams :: [String] -> [[String]]
groupAnagrams listaCadenas =
    elems $ foldr (\cadena -> insertWith (++) (sort cadena) [cadena]) empty listaCadenas


-----------------------------------------------------------------------------------------
-- 2. La función subsets recibe una lista de elementos únicos y devuelve el conjunto 
--    potencia.
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)


-----------------------------------------------------------------------------------------
-- 3. La función majorityElem recibe una lista y devuelve su elemento mayoritario.
majorityElem :: Eq a => [ a ] -> a
majorityElem [] = error "Lista vacia"
majorityElem (x:xs) = findMajority x xs 1

-- Función auxiliar que encuentra el elemento mayoritario en una lista utilizando 
-- un enfoque de conteo.
findMajority :: Eq a => a -> [ a ] -> Int  -> a
findMajority elem [] _ = elem
findMajority elem (x:xs) count
    | count == 0 = findMajority x xs 1
    | elem == x = findMajority elem xs (count + 1)
    | otherwise = findMajority elem xs (count - 1) 


-----------------------------------------------------------------------------------------
-- 4. La función coins recibe una lista de monedas de diferentes denominaciones y 
--    una cantidad total de dinero, y devuelve si es posible completar la cantidad 
--    usando únicamente ese tipo de monedas.   
coins :: [Int] -> Int -> Bool
coins _ 0 = True  
coins [] _ = False 
coins (coin:rest) amount
    | amount < 0 = False  
    | otherwise = coins rest amount || coins (coin:rest) (amount - coin)


-----------------------------------------------------------------------------------------
-- 5. Considera la siguiente definición de árbol binario:
data BST a = Empty | Node a ( BST a ) ( BST a ) deriving Show

-- Función auxiliar para encontrar el valor máximo entre los nodos de un BST.
maxNodeValue :: BST Int -> Int
maxNodeValue (Node v Empty Empty) = v
maxNodeValue (Node v leftSubTree Empty) = maximum [(maxNodeValue leftSubTree), v]
maxNodeValue (Node v Empty rightSubTree) = maximum [v, (maxNodeValue rightSubTree)]
maxNodeValue (Node v leftSubTree rightSubTree) = maximum [(maxNodeValue leftSubTree), v, (maxNodeValue rightSubTree)]

-- Función auxiliar para encontrar el valor mínimo entre los nodos de un BST.
minNodeValue :: BST Int -> Int
minNodeValue (Node v Empty Empty) = v
minNodeValue (Node v leftSubTree Empty) = minimum [(minNodeValue leftSubTree), v]
minNodeValue (Node v Empty rightSubTree) = minimum [v, (minNodeValue rightSubTree)]
minNodeValue (Node v leftSubTree rightSubTree) = minimum [(minNodeValue leftSubTree), v, (minNodeValue rightSubTree)]

-- La función isBST recibe un árbol binario y devuelve si es un árbol de búsqueda 
-- binario válido. 
isBST :: BST Int -> Bool
isBST (Empty) = True
isBST (Node _ Empty Empty) = True
isBST (Node v leftSubTree Empty) = ((maxNodeValue leftSubTree) < v) && (isBST leftSubTree)
isBST (Node v Empty rightSubTree) = (v < (minNodeValue rightSubTree)) && (isBST rightSubTree)
isBST (Node v leftSubTree rightSubTree) = ((maxNodeValue leftSubTree) < v) && (v < (minNodeValue rightSubTree)) && (isBST leftSubTree) && (isBST rightSubTree)


-----------------------------------------------------------------------------------------
-- 6. La función kthElem recibe un árbol de búsqueda binaria y un número entero k, y 
--    devuelve el k-ésimo valor más pequeño.
kthElem :: BST a -> Int -> a
kthElem Empty _ = error "Árbol vacío"
kthElem (Node v leftSubTree rightSubTree) k
  | k <= size leftSubTree = kthElem leftSubTree k
  | k == size leftSubTree + 1 = v
  | otherwise = kthElem rightSubTree (k - size leftSubTree - 1)
  where
    size Empty = 0
    size (Node _ left right) = 1 + size left + size right


