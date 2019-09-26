-- Procesamiento de una linea normal -> estructura necesaria para el IG
import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map) 
import Data.Char
import Data.List (sort,map,isSubsequenceOf) --isSubsequenceOf es necesaria
import System.IO


oneLine = "1 A series of escapades demonstrating the adage that what is good for the goose is also good for the gander , some of which occasionally amuses but none of which amounts to much of a story ."
lineProcesada = words (map toLower oneLine)

listValuesGood = ['a'..'z']

pertenece :: [Char] -> Char -> Bool
pertenece [] n = False
pertenece (x:xs) n | x == n = True
                   | x /= n = pertenece xs n

--Con esto parto la lista para tener la palabra y el npi que debo ir contando
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

-- listo....) primero debo registrar el tipo de documento (0,1..4) y contarlos para los np => Sumatoria = n
-- falta....) segundo aplicar remove_dups ( partirLista linea_del_documento) para obtener los npi => Sumatoria = ni

-- contarNp resive el primer string de la linea que es el tipo de resena, y lo almazena en una tupla
-- enviarle (head lineProcesada), con esto cuento los np
contarNp :: String -> (Int, Int, Int) -> (Int, Int, Int)
contarNp [] (a,b,c) = (a,b,c)
contarNp x (a,b,c) | (read x) < 2  = (a+1,b,c)
                   | (read x) > 2  = (a,b,c+1)
                   | (read x) == 2 = (a,b+1,c)

--Con esto cuento el N
contarN :: (Int, Int, Int) -> Int
contarN (0,0,0) = 0
contarN (a,b,c) = a+b+c

-- contarNpi resibe un [(String, String)] -> [(String, ( Int, Int, Int))] que se traduce a ('palabra',(las veces que aparece en..))
contarNpi :: [(String, String)] -> [(String, ( Int, Int, Int))] -> [(String, ( Int, Int, Int))]
contarNpi [] (x:xs) = (x:xs)
contarNpi [(y,ys)] [] = [(ys, contarNp y (0,0,0) )]
--contarNpi [(y,ys)] ((x,(a,b,c):xs) | isSubsequenceOf ys x == True = (x, contarNp y (a,b,c) ) : xs
--                                   | otherwise = (x,(a,b,c)) : contarNpi [(y,ys)] xs
estaFuncionanado = isSubsequenceOf "hola" "hola"