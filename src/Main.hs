module Main where

import System.IO
import System.Exit
import System.Directory
import Control.Monad
import Data.Set
import Automata
import Read

mPut :: String -> IO String
mPut text = do
    putStr text
    hFlush stdout
    getLine
    
aux :: Moore -> Mealy -> String -> String -> ([Alfabeto],[[Alfabeto]],Bool)
aux moore mealy tipo entrada 
  | tipo == "moore" =
    (traduceMoore moore entrada, traduceMooreCompleto moore entrada, acepta(automataMoore moore) entrada)
  | otherwise = 
  (traduceMealy mealy entrada, traduceMealyCompleto mealy entrada, acepta (automataMealy mealy) entrada)
  
main = do
  putStrLn "Leo automátas que se encuentran en la carpeta automatas"
  archivo <- mPut "Archivo donde se encuentra el automáta: "
  contents <- readFile $ "../automatas/"++archivo
  let values = Prelude.map createDuple $ Prelude.map miSplit $ removeBlank $ readLines $  contents
  if (length values) /= 8
    then do
    putStrLn "Lectura de Archivo Fallido.\nRecuerda poner '.' al final de tus declaraciones"
    exitFailure
    else do
    putStrLn "Lectura de Archivo Terminada"
  -- obtener datos del archivo
  let nEstados = getEstados values
  let nEntrada = getEntrada values
  let nSalida  = getSalida  values
  let nTrans   = getTrans   values
  let nInit    = getInit    values
  let nFinal   = getFinales values
  -- Crear el automáta
  let estados automata = nEstados
  let automata = Automata
        {estados = nEstados,
         alfabetoEntrada = nEntrada,
         alfabetoSalida = nSalida,
         transiciones = nTrans,
         inicial = nInit,
         finales = nFinal
        }
  -- Checar entre Moore y Mealy
  let tipo = getTipo values      
  menu automata values tipo
  
---
-- Menus
--
menu automata values tipo= do
  let otro = if tipo == "moore" then "mealy"
             else "moore"  
  putStrLn $ "\n-----\nEstamos trabajando con un traductor de tipo " ++ tipo
  putStrLn "Escoje una opción:\n1) Ver automáta\n2) Introducir una cadena"
  putStrLn $ "3) Traducirlo a uno de " ++ otro ++ "\n4) Salir\n---\n"
  leer <- getLine
  let opcion = (read leer :: Int)  
  let nRes     = getRes values
  let moore = Moore{
        automataMoore = automata,
        fRespuestas = nRes
        }
  let nResMealy = getResMealy values
  let mealy = Mealy{
        automataMealy = automata,
        fRespuestasM = nResMealy
        }
  let checkAut = if tipo == "moore" then checkMoore moore else checkMealy mealy
  if not checkAut then do
    putStrLn "El autómata NO esta bien definido.\n Saliendo..."
    exitFailure
    else do
    putStrLn "El autómata esta bien definido"
  case opcion of
    1 -> do
      if tipo == "moore" then do print moore else print mealy
    2-> do
      procesaEntrada automata moore mealy tipo 
    3 -> do
      if tipo == "moore" then do
        let meal = toMealy moore
        menu2 automata moore meal "mealy"
        else do
        --let mor = toMoore mealy
        --menu2 automata mor mealy "moore"
        let splits2 = splitEstados' (estados automata) (transiciones automata) (fRespuestasM mealy)
        let automata2 = Automata{
              estados = mealyToMooreEstados mealy,
              alfabetoEntrada = (alfabetoEntrada (automataMealy mealy)),
              alfabetoSalida = (alfabetoSalida (automataMealy mealy)),
              transiciones =  toList $ fromList $nuevasTransiciones splits2 (transiciones automata) (fRespuestasM mealy),
              inicial = nuevoInicial mealy,
              finales = nuevosFinales mealy
              }
        let nResMoore = nuevasRespuestas (estados automata) (transiciones automata) (fRespuestasM mealy)
        let mor = Moore{
              automataMoore = automata2,
              fRespuestas = nResMoore
                }
        menu2 automata2 mor mealy "moore"
    4 -> exitFailure
    _ -> do
      putStrLn "ERROR: Opción no definida"
  menu automata values tipo


menu2 automata moore mealy tipo= do
  let otro = if tipo == "moore" then "mealy"
             else "moore"  
  putStrLn $ "\n-----\nEstamos trabajando con un traductor de tipo " ++ tipo
  putStrLn "Escoje una opción:\n1) Ver automáta\n2) Introducir una cadena"
  putStrLn $ "3) Guardar automata\n4) Volver al anterior\n5) Salir\n---\n"
  leer <- getLine
  let opcion = (read leer :: Int)
  case opcion of
    1 -> do if tipo == "moore" then do print moore else print mealy
            menu2 automata moore mealy tipo
    2 -> do procesaEntrada automata moore mealy tipo
            menu2 automata moore mealy tipo
    3 -> do
      file <- mPut "El Archivo se guardara en la carpea automatas\n. Nombre del archivo: "
      fileExist <- doesFileExist$ "../automatas/"++file
      if fileExist then do
        putStrLn "ERROR: El archivo ya existe, intenta con otro nombre..."
        else do
        putStrLn "El archivo no existe, Guardando..."
        let cadena = automataToString automata
        let salida = if tipo == "moore" then
              "Moore.\nRespuesta = " ++ mooreResToString (fRespuestas moore)
              else  "Mealy.\nRespuesta = " ++ mealyResToString (fRespuestasM mealy)
        writeFile ("../automatas/"++file) $ salida ++ cadena
        putStrLn "Se guardo con exito"
    4 -> return ()
    5 -> exitFailure
    _ -> do putStrLn "ERROR: Opción no definida"
            menu2 automata moore mealy tipo
  
procesaEntrada :: Automata -> Moore -> Mealy -> String  -> IO ()
procesaEntrada automata moore mealy tipo  = do
  entrada <- mPut "Cadena a procesar: "
  let valida = checkCadena automata entrada
  if not valida then do
    putStrLn "ERROR: Cadena definida sobre otro alfabeto"
    else  do
    putStrLn "==="
    putStrLn "Cadena Válida"
    let trans    = transita automata entrada
    let(traducc, fullTraducc, aceptada) = aux moore mealy tipo entrada        
    if (length trans) /= (length entrada)+1 then do
      putStrLn "No se pudo procesar toda la cadena. No es aceptada"
      putStrLn "transiciones logradas:"
      print trans
      putStrLn "traduccion lograda:"
      print fullTraducc
      else do
      putStrLn "Se pudo procesar toda la cadena\nSe muestran todas las transiciones:"
      print trans
      putStrLn "Salida del traductor:"
      print traducc
      putStrLn "traduccion lograda:"
      print fullTraducc
      if (aceptada == True) then do
        putStrLn $entrada ++" es una cadena aceptada"
        else do putStrLn $entrada ++" NO es una cadena aceptada"
    putStrLn "==="
