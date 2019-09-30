-- area de pruebas
import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map) 
import Data.Char
import Data.List (sort,map,isSubsequenceOf) --isSubsequenceOf es necesaria
import System.IO
-- formula de calculo IG(ki)

logEnBase :: Float -> Float -> Float
logEnBase b x = (log x) / (log b)

log2 x = if x == 0 then 0 else log(x)/log(2)

f x = x * log2 x

sumatoriaF :: [Float] -> Float
sumatoriaF [] = 0
sumatoriaF (x:xs) = f(x) + sumatoriaF (xs)

sumatoriaFF :: [Float] -> Float -> Float
sumatoriaFF [] n = 0
sumatoriaFF (x:xs) n = f(x/n) + sumatoriaFF (xs) n

-- calH [np] n

calH :: [Float] -> Float -> Float
calH [] n = 0
calH (x:xs) n = -1 * sumatoriaFF (x:xs) n

auxCalHki :: [Float] -> [Float] -> Float
auxCalHki [] [] = 0
auxCalHki (x:xs) (y:ys) = f(x-y) + auxCalHki xs ys

-- calHki [np] [nip] n ni

calHki :: [Float] -> [Float] -> Float -> Float -> Float
calHki [] [] n ni = 0
calHki (x:xs) (y:ys) n ni = (-1 * (sumatoriaF (y:ys))/n)+(-1 * (auxCalHki (x:xs) (y:ys) )/n ) + ((f ni + f(n-ni))/n)

-- indiceGanancia [np] [nip] n ni

indiceGanancia :: [Float] -> [Float] -> Float -> Float -> Float
indiceGanancia [] [] n ni = 0
indiceGanancia (x:xs) (y:ys) n ni = calH (x:xs) n - calHki (x:xs) (y:ys) n ni

oneLine = "1 A series of escapades demonstrating the adage that what is good for the goose is also good for the gander , some of which occasionally amuses but none of which amounts to much of a story ."
lineProcesada = words (map toLower oneLine)

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

listaParaIG = [(0.9911,"oferta"),(0.6305,"oportunidad"),(0.3244,"decision"),(0.2516,"respuesta"),(0.0975,"salud"),(0.0294,"otro"),(0.0183,"urgente")]
-- esta instruccion es para obtener los m mejores
partirListaM :: [a] -> Int -> [a]
partirListaM [] m = []
partirListaM x 0 = []
partirListaM (x:xs) m = x : partirListaM xs (m-1)

invertirListaM::Ord a=>[a]->[a]
invertirListaM [ ] = [ ]
invertirListaM (x:xs) = (invertirListaM xs)++[x]

mMejores :: [(Double, String)] -> Int -> [(Double, String)]
mMejores [] m = [(0,"Vacio")]
mMejores (x:xs) m = partirListaM ( invertirListaM (sort (x:xs)) ) m

mPeores :: [(Double, String)] -> Int -> [(Double, String)]
mPeores [] m = [(0,"Vacio")]
mPeores (x:xs) m = partirListaM ( sort (x:xs) ) m