import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map)
import Data.Char
import Data.List (sort,map)
import System.IO

run = sort [5,2,8,3,4]
-- tengo problemas para obtener los valores np, nip = no puedo usar el hashmap. Pero voy a demostrar que puedo hacer los calculos

type Estado = HashMap [Char] Int

main :: IO ()
main = do 
       putStrLn ">> Bienvenido"
       mainloop (fromList[])

mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "procesar" -> do
                putStrLn ">>> No funciona..."
                putStrLn ">>> Nombre del archivo a procesar: "
                nombreArchivo <- getLine
                inh <- openFile nombreArchivo ReadMode
                -- aqui debo guardarlo como un nuevo estado, el cual debo modificar
                nuevoestado <- cargarMof inh estado
                hClose inh
                putStrLn $ "Archivo " ++ nombreArchivo ++ " fue procesado sin problemas" 
                mainloop nuevoestado
     "mejores" -> do
                putStrLn ">>> Cual es su limite: "
                valorM <- getLine
                putStrLn $ "Los mejores " ++ valorM ++ " son: "
                -- putStrLn mMejores listaParaIG (read valorM)
                mainloop estado
     "peores"  -> do
                putStrLn ">>> comando en construccion: "
                mainloop estado
     "guardar" -> do
               putStrLn ">>> Nombre archivo salida: "
               nombreArchivo <- getLine
               --neesito modificarlo para que guarde los m mejores con su respectivo IG
               outh <- openFile nombreArchivo WriteMode
               descargar outh (sort (toList estado))
               hClose outh
               -- putStrLn $ "Comando guardar desactivado"
               mainloop estado     
     "clin" -> do
                let (nuevoestado, salida)= contar_linea (tail tokens) estado
                putStrLn salida
                mainloop nuevoestado
     "borrar" -> do
                   let (nuevoestado, salida)= cmd_borrar (tail tokens) estado
                   putStrLn salida
                   mainloop nuevoestado
                   -- putStrLn $ "Comando borrar desactivado"
                   -- mainloop estado
     "imp" -> do
                 let (nuevoestado, salida) = cmd_imp estado
                 putStrLn salida
                 mainloop nuevoestado
     "fin" -> do
                 putStrLn "Saliendo..."
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado

-- función que implementa el comando contar_linea
contar_linea :: [String] -> Estado -> (Estado, String) 
contar_linea tokens estado = (foldl contar_token estado tokens, "contar_linea" )

contar_token :: Estado -> String -> Estado
contar_token estado tok = case lookup tok estado of
                               Nothing -> insert tok 1 estado
                               Just valor -> insert tok (valor+1) estado
  
                 
-- función que implementa el comando def
-- cmd_def::[String] -> Estado -> (Estado, String)
-- cmd_def tokens estado = (nuevoestado, mensaje)
  -- where nuevoestado = insert (tokens!!0) (tokens!!1) estado
        -- mensaje = tokens!!0 ++ " fue definida"

-- función que implementa el comando borrar
cmd_borrar::[String] -> Estado -> (Estado, String)
cmd_borrar [] estado = (estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = if member v estado 
                               then (delete v estado, v ++ " borrado")
                               else (estado, v ++ " no aparece")

-- función que maneja un comando desconocido
cmd_desconocido ::
      String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False,estado,mensaje)
  where mensaje = "Comando desconocido ("++ cmd ++"): '" 
                                         ++ comando ++ "'"

-- función que implementa el comando imp
cmd_imp :: Estado -> (Estado, String)
cmd_imp estado = (estado, show estado)

cargar :: Handle -> Estado -> IO Estado
cargar inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       --putStr inpStr --aqui se lee linea por linea
                       let nuevoestado = foldl contar_token estado (words (map toLower inpStr))
                       cargar inh nuevoestado


-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs


--------------------------------------------------------------------------------------------------
----------------modificaciones--------------------------------------------------------------------
cargarMof :: Handle -> Estado -> IO Estado
cargarMof inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       putStrLn inpStr --aqui se lee linea por linea
                       let nuevoestado = foldl contar_token estado (words (map toLower inpStr))
                       cargarMof inh nuevoestado

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

listaOrdenada = sort [(8.9,"A"),(23.5,"Avd"),(12.4,"Atfdf"),(1.3,"Aas"),(3,"Aasd")]
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

---------------------------------------------------------------------------------------------------
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

--- lista para probar los mejores m
listaParaIG = [(0.9911,"oferta"),(0.6305,"oportunidad"),(0.3244,"decision"),(0.2516,"respuesta"),(0.0975,"salud"),(0.0294,"otro"),(0.0183,"urgente")]