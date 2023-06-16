module Automata where

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
  
---
-- MOORE
--
-- | checkMoore: No verifica la correctitud del automáta, sino
--               que las funciones de transicion y respuesta,
--              así como el Inicial y Finales esten definidos correctamente.
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

--toMoore :: Mealy -> Moore
--toMoore m = 
---
-- AMBOS
---
checkResAux :: [Alfabeto] -> [Alfabeto] -> Bool
checkResAux [] _ = True
checkResAux (x:xs) l
  | elem x l = checkResAux xs l
  | otherwise = False
