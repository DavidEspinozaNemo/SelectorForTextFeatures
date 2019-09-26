-- Procesamiento de una linea normal -> estructura necesaria para el IG
import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map)
import Data.Char
import Data.List (sort,map)
import System.IO


oneLine = "1 A series of escapades demonstrating the adage that what is good for the goose is also good for the gander , some of which occasionally amuses but none of which amounts to much of a story ."
lineProcesada = words (map toLower oneLine)

listValuesGood = ['a'..'z']

pertenece :: [Char] -> Char -> Bool
pertenece [] n = False
pertenece (x:xs) n | x == n = True
                   | x /= n = pertenece xs n

partirLista :: [String] -> [(String, String)]
partirLista [x:_] = []
partirLista (x:y:xs) = (x, y) : partirLista (x:xs)

-- stackoverflow
-- con esto remuevo los elementos repetidos de la lista de valores. Tengo que comparar un elemento contra todos.
remove_dups :: (Ord a, Eq a) => [a] -> [a]
remove_dups xs = remove $ sort xs
  where
    remove [] = []
    remove [x] = [x]
    remove (x1:x2:xs)
        | x1 == x2 = remove(x1:xs)
        | otherwise = x1 : remove(x2:xs)
-- otras funciones

-- por ahora) primero debo registrar el tipo de documento (0,1..4) y contarlos para los np => Sumatoria = n
-- por ahora) segundo aplicar remove_dups ( partirLista linea_del_documento) para obtener los npi => Sumatoria = ni