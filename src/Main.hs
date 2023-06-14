module Main where

import System.IO
import Control.Monad
import Data.List.Split
import Automata
import Data.Char

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
  | length x == 1 = if head x == "Moore"
                    then (head x, "1")
                    else (head x, "ERROR")
  | length x == 2 = (a, b)
  | otherwise     = ("ERROR", "ERROR")
    where
      a = head x
      b = head $ drop 1 x

main = do
  let automata = Moore 
  archivo <- getLine
  contents <- readFile $ "../samples/"++archivo
  print .  map createDuple $ map miSplit $ removeBlank $ readLines $  contents
  -- Usar constructores para darle valores al
  --    automata. Hacer la interfaz gráfica
