module Main where

import System.IO
import System.Exit
import Control.Monad
import Automata
import Read

  
main = do
  archivo <- getLine
  contents <- readFile $ "../automatas/"++archivo
  let values = map createDuple $ map miSplit $ removeBlank $ readLines $  contents
  if (length values) /= 8
    then do
    putStrLn "Archivo Fallida"
    exitFailure
    else do
    putStrLn "Archivo Valido"
  -- obtener datos del archivo
  let nEstados = getEstados values
  let nEntrada = getEntrada values
  let nSalida  = getSalida  values
  let nTrans   = getTrans   values
  let nRes     = getRes     values
  let nInit    = getInit    values
  let nFinal   = getFinales values
  -- Crear el automáta
  let estados automata = nEstados
  let automata = Moore
        {estados = nEstados,
         alfabetoEntrada = nEntrada,
         alfabetoSalida = nSalida,
         transiciones = nTrans,
         fRespuestas = nRes,
         inicial = nInit,
         finales = nFinal
        }
  let checkAut = checkMoore automata
  print checkAut    
  entrada <- getLine -- Verificar entrada
  let valida = checkCadena automata entrada
  if not valida then do
    putStrLn "Cadena Fallida"
    exitFailure
    else  do
    putStrLn "Cadena Válida"
  let trans    = transitaMoore automata entrada
  let traducc  = traduceMoore automata entrada
  if (length trans) /= (length entrada)+1 then
    do
      putStrLn "No se pudo procesar toda la cadena"
    else
    putStrLn "Se pudo procesar toda la cadena"
  let aceptada = aceptaMoore automata entrada
  print automata
  print trans
  print traducc
  print aceptada
    -- Menu
    -- putStrLn "Que deseas hacer: \n1) Imprimir Automata\n2) Introducir Cadena"
    
