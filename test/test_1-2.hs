import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map)
import Data.Char
import Data.List (sort,map)
import System.IO

incTupla3 :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
incTupla3 (x, y, z) = (x + 1, y + 1, z + 1)

--ConteoClases :: (Integer, Integer, Integer) -> Integer -> (Integer, Integer, Integer)
conteoClases :: (Int, Int, Int) -> Int -> (Int, Int, Int)
conteoClases (a,b,c) x | x<2 = (a+1, b, c)
                       | x==2 = (a,b+1,c)
                       | x>2 = (a,b,c+1)