module Read where

import Data.Char
import Data.List.Split
import Automata

auxRead :: Char -> [[Char]] -> [[Char]]
auxRead '.' l = []:l
auxRead '\n' l = l
auxRead '\t' l = l
auxRead c []   = [[c]]
auxRead c (x:xs) = (c:x):xs

readLines :: [Char] -> [[Char]]
readLines = foldr auxRead []

removeBlank :: [[Char]] -> [[Char]]
removeBlank [] = []
removeBlank (x:xs) = removeBlankAux x : removeBlank xs

removeBlankAux :: [Char] -> [Char]
removeBlankAux [] = []
removeBlankAux (x:xs)
  | x == ' ' = removeBlankAux xs
  | otherwise = x:removeBlankAux xs

miSplit :: [Char] -> [[Char]]
miSplit x = splitOn "=" x

createDuple :: [[Char]] -> ([Char], [Char])
createDuple [[]] = ([],[])
createDuple x
  | length x == 1 = if cabeza == "moore"
                    then (cabeza, "1")
                    else if cabeza == "mealy"
                         then  (cabeza, "2")
                    else (cabeza, "ERROR")
  | length x == 2 = (cabeza, b)
  | otherwise     = ("ERROR", "ERROR")
  where
    cabeza = map toLower$ head x
    b = head $ drop 1 x
      
getEstados :: [([Char], [Char])] -> [Estado]
getEstados [] = []
getEstados (("estados", x):_) = splitOn "," x
getEstados (_:xs) = getEstados xs

getEntrada :: [([Char], [Char])] -> [Alfabeto]
getEntrada [] = []
getEntrada (("entrada", x):_) = x
getEntrada (_:xs) = getEntrada xs

getSalida :: [([Char], [Char])] -> [Alfabeto]
getSalida [] = []
getSalida (("salida", x):_) = x
getSalida (_:xs) = getSalida xs

decompose :: [[Char]] -> [[[Char]]]
decompose []   = []
decompose (x:xs) = (splitOn "->" x):decompose xs

setTrans :: [[[Char]]] -> [Transicion]
setTrans [] = []
setTrans (x:xs) 
  | length x == 3 = (a,b,c):setTrans xs
  | otherwise     = [("ERROR", '0', "ERROR")]
  where
    a = head x
    b = head $ head $ drop 1 x
    c = head $ drop 2 x

getTrans :: [([Char], [Char])] -> [Transicion]
getTrans [] = []
getTrans (("transicion", x):_) = setTrans $ decompose ( splitOn "," x)
getTrans (_:xs) = getTrans xs

-- | getRes: Permite leer los valores del archivo de un
--         automáta de Moore.
getRes :: [([Char], [Char])] -> [Respuesta]
getRes [] = []
getRes (("respuesta", x):_) = setRes $ decompose (splitOn "," x)
getRes (_:xs) = getRes xs

setRes :: [[[Char]]] -> [Respuesta]
setRes [] = []
setRes (x:xs) 
  | length x == 2 = (a,b):setRes xs
  | otherwise     = [("ERROR", "0")]
  where
    a = head x
    b = head $ drop 1 x

getResMealy :: [([Char], [Char])] -> [RespuestaMealy]
getResMealy [] = []
getResMealy (("respuesta", x):_) = setResM $ decompose (splitOn "," x)
getResMealy (_:xs) = getResMealy xs

setResM :: [[[Char]]] -> [RespuestaMealy]
setResM [] = []
setResM (x:xs) 
  | length x == 3 = (a,b,c):setResM xs
  | otherwise     = [("ERROR", '0', "ERROR")]
  where
    a = head x
    b = head $ head $ drop 1 x
    c = head $ drop 2 x
    
getInit :: [([Char], [Char])] -> Estado
getInit [] = []
getInit (("inicial", x):_) = x
getInit (_:xs)             = getInit xs


getFinales :: [([Char], [Char])] -> [Estado]
getFinales [] = []
getFinales (("finales", x):_) = splitOn "," x
getFinales (_:xs) = getFinales xs


getTipo :: [([Char], [Char])] -> [Char]
getTipo [] = []
getTipo (("moore", x):_) = "moore"
getTipo (("mealy", x):_) = "mealy"
getTipo (_:xs) = getTipo xs
