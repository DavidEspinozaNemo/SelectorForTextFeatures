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