module Automata where

import Data.List
import Data.Set

type Estado = [Char]
type Alfabeto = Char
type Transicion = (Estado, Alfabeto,
                   Estado)
type Respuesta  =  (Estado, [Alfabeto])
type RespuestaMealy  =
  (Estado, Alfabeto, [Alfabeto])

data Automata = Automata{
  estados :: [Estado],
  alfabetoEntrada :: [Alfabeto],
  alfabetoSalida  :: [Alfabeto],
  transiciones :: [Transicion],
  inicial :: Estado,
  finales :: [Estado]
  } deriving Show
  
data Moore = Moore{  
  fRespuestas :: [Respuesta],
  automataMoore :: Automata
  } deriving Show

data Mealy = Mealy{
  fRespuestasM :: [RespuestaMealy],
  automataMealy :: Automata
  } deriving Show

---
--  AUTOMATA
---
-- | checkCadena: Verifica que la cadena a procesar este bien construida
--               de acuerdo al alfabeto del automáta.
checkCadena :: Automata -> [Alfabeto] -> Bool
checkCadena m s = checkCadenaAux
  (alfabetoEntrada m) s
  where checkCadenaAux _ [] = True
        checkCadenaAux l (x:xs)
          | elem x l  = checkCadenaAux l xs
          | otherwise = False

-- | checkMoore: No verifica la correctitud del automáta, sino
--               que las funciones de transicion, el
--              Inicial y Finales esten definidos correctamente.
checkAutomata :: Automata -> Bool
checkAutomata m = checkTrans && checkInit && checkFin
  where
    etats = estados m
    checkTrans = checkTransiciones (transiciones m)  etats (alfabetoEntrada m)
    checkInit  = elem (inicial m) etats
    checkFin   = checkFinal (finales m) etats

-- | checkTransiciones: Verifica que las transiciones esten
--                    definidas sobre el alfabeto de Entrada
--                    y los estados del automáta.
checkTransiciones :: [Transicion] -> [Estado] -> [Alfabeto] -> Bool
checkTransiciones [] _ _ = True
checkTransiciones ((a,b,c):xs) le la
  | elem a le && elem c le && elem b la = checkTransiciones xs le la
  | otherwise                           = False

-- | checkFinal: Verifica que los finales sean estados definidos
checkFinal :: [Estado] -> [Estado] -> Bool
checkFinal [] _ = True
checkFinal (x:xs) l = elem x l && checkFinal xs l


-- | getTransicion: Devuelve el estado al que el automáta debe
--                 transicionar.
getTransicion :: Estado -> [Transicion] -> Alfabeto -> Maybe Estado
getTransicion e [] _ = Nothing
getTransicion e ((x,y,z):ts) c
  | e == x && y == c = Just z
  | otherwise        = getTransicion e ts c

-- | transita: dado el inicial el automata procesa la cadena
transita :: Automata -> [Alfabeto] -> [Estado]
transita m s = inicial m :transitaAux (inicial m) (transiciones m) s

-- | transitaAux: función auxiliar para hacer las transiciones
transitaAux ::  Estado -> [Transicion] -> [Alfabeto] ->  [Estado]
transitaAux _ _ [] = []
transitaAux e l (c:cs)
  | sig == Nothing = []
  | otherwise     = nSig: (transitaAux nSig l cs)
  where
    sig  = getTransicion e l c
    nSig =  deleteJust sig

deleteJust :: Maybe a -> a
deleteJust (Just a) = a

esFinal :: Automata -> Estado -> Bool
esFinal m e = elem e $ finales m

-- | acepta :: determina si una cadena es aceptada en el automáta o no.
acepta :: Automata -> [Alfabeto] -> Bool
acepta m s = esFinal m (last list) && length list == length s +1
  where list = transita m s

automataToString :: Automata -> String
automataToString m = "Inicial = " ++ (inicial m) ++ ".\nEstados = "++estadosToString (estados m)
  ++ "Finales = " ++ estadosToString (finales m) ++ "Entrada = " ++ (alfabetoEntrada m)
  ++ ".\nSalida = " ++ (alfabetoSalida m) ++".\nTransicion = " ++ transicionesToString (transiciones m)
  
estadosToString :: [Estado] -> String
estadosToString (x:[]) = x++".\n"
estadosToString (x:xs) = x++","++estadosToString xs

transicionesToString :: [Transicion] -> String
transicionesToString ((a,b,c):[]) = a ++"->"++ (b:[]) ++ "->" ++ c
  ++ ".\n"
transicionesToString ((a,b,c):xs) = a ++"->"++ (b:[]) ++ "->" ++ c
  ++ "," ++ transicionesToString xs
---
-- MOORE
--
-- | checkMoore: No verifica la correctitud del automáta, sino
--               que las funciones de transicion y respuesta,
--              así como el Inicial y Finales esten definidos correctamente.
mooreResToString :: [Respuesta] -> String
mooreResToString ((a,b):[]) = a++"->"++b++".\n"
mooreResToString ((a,b):xs) = a++"->"++b++", " ++ mooreResToString xs

checkMoore :: Moore -> Bool
checkMoore m = checkRes && checkAutomata (automataMoore m) 
  where
    checkRes   = checkRespuestas (fRespuestas m) (estados (automataMoore m)) (alfabetoSalida (automataMoore m))

-- | checkRespuesta: Verifica que las respuestas de Moore esten bien definidas
checkRespuestas :: [Respuesta] -> [Estado] -> [Alfabeto] -> Bool
checkRespuestas [] _ _ = True
checkRespuestas ((e,a):xs) le la
  | elem e le && checkResAux a la = checkRespuestas xs le la
  | otherwise              = False
  
-- | getRespuesta: Devuelve el carácter correspondiente al
--                automata dado el estado i.e automáta de Moore.
getRespuesta :: Estado -> [Respuesta] -> [Alfabeto] 
getRespuesta e [] = '-':[]
getRespuesta e ((a,b):xs)
  | e == a     = b
  | otherwise  = getRespuesta e xs

-- | traduceMoore: Obtiene la traudcción del automáta
traduceMoore :: Moore -> [Alfabeto] -> [Alfabeto]
traduceMoore m s = last $ traduceMooreCompleto m s
  --getRespuesta (last (transita (automataMoore m) s)) (fRespuestas m)

traduceMooreCompleto :: Moore -> [Alfabeto] -> [[Alfabeto]]
traduceMooreCompleto m s = auxCompleto (transita (automataMoore m) s) (fRespuestas m)

auxCompleto :: [Estado] -> [Respuesta] -> [[Alfabeto]]
auxCompleto [] _ = []
auxCompleto (x:xs) l = (getRespuesta x l):(auxCompleto xs l)

toMealy :: Moore -> Mealy
toMealy m = Mealy{ automataMealy = (automataMoore m),
                   fRespuestasM  = mealyRes
                 }
  where mealyRes = toMealyAux (transiciones (automataMoore m)) (fRespuestas m)

toMealyAux :: [Transicion] -> [Respuesta] -> [(Estado, Alfabeto, [Alfabeto])]
toMealyAux [] _     = []
toMealyAux ((a,b,c):xs) l = (a,b,d):toMealyAux xs l
  where d = getRespuesta c l
  
---
-- MEALY
---
mealyResToString :: [RespuestaMealy] -> String
mealyResToString ((a,b,c):[]) = a ++ "->" ++ (b:[]) ++ "->" ++c ++ ".\n"
mealyResToString ((a,b,c):xs) = a ++ "->" ++ (b:[]) ++ "->" ++ c ++ "," ++ mealyResToString xs
  
checkMealy :: Mealy -> Bool
checkMealy m = checkAutomata (automataMealy m) && checkResMealy
  where
    checkResMealy = checkRespuestasMealy (fRespuestasM m) (estados (automataMealy m))
      (alfabetoEntrada (automataMealy m)) (alfabetoSalida (automataMealy m))

-- | checkRespuesta: Verifica que las respuestas esten bien definidas
checkRespuestasMealy :: [RespuestaMealy] -> [Estado] -> [Alfabeto] -> [Alfabeto] -> Bool
checkRespuestasMealy [] _ _ _ = True
checkRespuestasMealy ((e,a,as):xs) le laEntrada laSalida
  | elem e le && elem a laEntrada && checkResAux as laSalida
  = checkRespuestasMealy xs le laEntrada laSalida
  | otherwise              = False
    

getRespuestaMealy :: Estado -> Alfabeto -> [RespuestaMealy] -> [Alfabeto]
getRespuestaMealy e _ [] = "-"
getRespuestaMealy e c ((a,b,d):xs)
  | e == a && c == b = d
  | otherwise        = getRespuestaMealy e c xs  

traduceMealy :: Mealy -> [Alfabeto] -> [Alfabeto]
traduceMealy m s =  last $ traduceMealyCompleto m s

traduceMealyCompleto :: Mealy -> [Alfabeto] -> [[Alfabeto]]
traduceMealyCompleto m s = auxCompletoMealy  (transita (automataMealy m) s) (fRespuestasM m) s

auxCompletoMealy :: [Estado] -> [RespuestaMealy] -> [Alfabeto] -> [[Alfabeto]]
auxCompletoMealy _ _ [] = []
auxCompletoMealy [] _ _ = []
auxCompletoMealy (x:xs) l (c:cs) = (getRespuestaMealy x c l):(auxCompletoMealy xs l cs)
                    
mealyToMooreEstados :: Mealy -> [Estado]
mealyToMooreEstados m = toList $ fromList $ nuevosEstados e t r
  where
    automata = automataMealy m
    e = estados automata
    t = transiciones automata
    r = fRespuestasM m

nuevosEstados :: [Estado] -> [Transicion] -> [RespuestaMealy] -> [Estado]
nuevosEstados [] _ _ = []
nuevosEstados (x:xs) t r = (nuevoEstado x t r)++ nuevosEstados xs t r

nuevoEstado :: Estado-> [Transicion] -> [RespuestaMealy] -> [Estado]
nuevoEstado e l r
  | length (splitEstado e l r) == 0 = [e]
  | otherwise                       = auxEstados (splitEstado e l r) 

auxEstados :: [(Estado, [Alfabeto])] -> [Estado]
auxEstados [] = []
auxEstados ((a,b):xs) = (a++b):auxEstados xs

splitEstados :: [Estado] -> [Transicion] -> [RespuestaMealy] -> [[(Estado, [Alfabeto])]]
splitEstados [] _ _ = []
splitEstados (x:xs) l r = splitEstado x l r : splitEstados xs l r

splitEstados' :: [Estado] -> [Transicion] -> [RespuestaMealy] -> [(Estado, Alfabeto, Estado, [Alfabeto])]
splitEstados' [] _ _ = []
splitEstados' (x:xs) l r = splitEstado' x l r ++ splitEstados' xs l r

nuevasTransiciones :: [(Estado, Alfabeto, Estado, [Alfabeto])] -> [Transicion] -> [RespuestaMealy] -> [Transicion]
nuevasTransiciones [] _ _ = []
nuevasTransiciones ((a,b,c,s):xs) t r
  | length (splitEstado a t r) == 0 = [(a, b, (c++s))] ++ nuevasTransiciones xs t r
  | otherwise                       =  auxPrueba (splitEstado a t r) b (c++s) ++ nuevasTransiciones xs t r

auxPrueba :: [(Estado, [Alfabeto])] -> Alfabeto -> [Alfabeto] ->[Transicion]
auxPrueba [] _ _ = []
auxPrueba ((a,b):xs) c s = [((a++b), c, s)]++auxPrueba xs c s
  
splitEstado' :: Estado -> [Transicion] -> [RespuestaMealy] -> [(Estado, Alfabeto, Estado, [Alfabeto])]
splitEstado' e [] _ = []
splitEstado' x ((a,b,c):ys) l
  | x == c = (a, b, x, f):splitEstado' x ys l
  | otherwise = splitEstado' x ys l
  where f = getEstadoAlfb (a,b,c) l

splitEstado :: Estado -> [Transicion] -> [RespuestaMealy] -> [(Estado, [Alfabeto])]
splitEstado e [] _ = []
splitEstado x ((a,b,c):ys) l
  | x == c = (x, f):splitEstado x ys l
  | otherwise = splitEstado x ys l
  where f = getEstadoAlfb (a,b,c) l

-- | getEstadoAlfb: dado el estado el origen de una transicion obtenemos
--                la salida asociada a dicha transición.
getEstadoAlfb :: Transicion -> [RespuestaMealy] -> [Alfabeto]
getEstadoAlfb _ [] = []
getEstadoAlfb (a,b,c) ((d,e,f):xs)
  | a == d && b == e = f
  | otherwise        = getEstadoAlfb (a,b,c) xs


nuevasRespuestas :: [Estado] -> [Transicion] -> [RespuestaMealy] -> [Respuesta]
nuevasRespuestas [] _ _ = []
nuevasRespuestas (x:xs) t r = nuevaRespuesta x t r ++ (nuevasRespuestas xs t r)

nuevaRespuesta :: Estado -> [Transicion] -> [RespuestaMealy] -> [Respuesta]
nuevaRespuesta x t r
  | (length (splitEstado x t r)) == 0 = [(x, "_")]
  | otherwise                 =  nuevaRespuestaAux (splitEstado x t r) 

nuevaRespuestaAux :: [(Estado, [Alfabeto])] -> [Respuesta]
nuevaRespuestaAux [] = []
nuevaRespuestaAux ((a,b):xs) = ((a++b), b):nuevaRespuestaAux xs

nuevoInicial :: Mealy -> Estado
nuevoInicial m = a++b
  where automata = automataMealy m
        (a,b) = head $splitEstado (inicial automata) (transiciones automata) (fRespuestasM m)
        
nuevosFinales :: Mealy -> [Estado]
nuevosFinales m = nuevosFinalesAux l
  where automata = automataMealy m
        l = splitEstados (finales automata) (transiciones automata) (fRespuestasM m)

nuevosFinalesAux :: [[(Estado, [Alfabeto])]] -> [Estado]
nuevosFinalesAux [] = []
nuevosFinalesAux (l:ls) = (auxList l) ++ (nuevosFinalesAux ls)

auxList :: [(Estado, [Alfabeto])] -> [Estado]
auxList [] = []
auxList ((a,b):xs) = (a++b):auxList xs
---
-- AMBOS
---
checkResAux :: [Alfabeto] -> [Alfabeto] -> Bool
checkResAux [] _ = True
checkResAux (x:xs) l
  | elem x l = checkResAux xs l
  | otherwise = False

