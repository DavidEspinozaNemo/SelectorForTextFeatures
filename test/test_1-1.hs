-- Pruebas, 1.1 conversion a minusculas
import Data.List
import System.IO
import Data.Char

letterListLowercase = ['a','a'..'z']
letterListUppercase = ['A','B'..'Z']
letterA = 'A'
lettera1 = 'a'
lettera2 = 'a'

printStringDavid :: String -> [Char]

printStringDavid " " = [' ']
printStringDavid (X:[]) = [X]
printStringDavid (X:Xs) = [X] ++ printStringDavid Xs