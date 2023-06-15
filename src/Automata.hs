module Automata where

type Estado = [Char]
type Alfabeto = Char
type Transicion = (Estado, Alfabeto, Estado)
type Respuesta  =  (Estado, Alfabeto)
type RespuestaMealy  =  (Estado, Alfabeto, Alfabeto)

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

checkCadena :: Automata -> [Alfabeto] -> Bool
checkCadena m s = checkCadenaAux (alfabetoEntrada m) s
  where checkCadenaAux _ [] = True
        checkCadenaAux l (x:xs)
          | elem x l  = checkCadenaAux l xs
          | otherwise = False
          
-- | checkMoore: No verifica la correctitud del automáta, sino
--               que las funciones de transicion y respuesta,
--              así como el Inicial y Finales esten definidos correctamente.
checkMoore :: Moore -> Bool
checkMoore m = checkTrans && checkRes && checkInit && checkFin
  where
    etats = estados (automataMoore m)
    checkTrans = checkTransiciones (transiciones (automataMoore m))  etats (alfabetoEntrada (automataMoore m))
    checkRes   = checkRespuestas (fRespuestas m) etats (alfabetoSalida (automataMoore m))
    checkInit  = elem (inicial (automataMoore m)) etats
    checkFin   = checkFinal (finales (automataMoore m)) etats
   
    
-- | checkTransiciones: Verifica que las transiciones esten
--                    definidas sobre el alfabeto de Entrada
--                    y los estados del automáta.
checkTransiciones :: [Transicion] -> [Estado] -> [Alfabeto] -> Bool
checkTransiciones [] _ _ = True
checkTransiciones ((a,b,c):xs) le la
  | elem a le && elem c le && elem b la = checkTransiciones xs le la
  | otherwise                           = False

-- | checkRespuesta: Verifica que las respuestas esten bien definidas
checkRespuestas :: [Respuesta] -> [Estado] -> [Alfabeto] -> Bool
checkRespuestas [] _ _ = True
checkRespuestas ((e,a):xs) le la
  | elem e le && elem a la = checkRespuestas xs le la
  | otherwise              = False

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

-- | getRespuesta: Devuelve el carácter correspondiente al
--                automata dado el estado i.e automáta de Moore.
getRespuesta :: Estado -> [Respuesta] -> Alfabeto 
getRespuesta e [] = '-'
getRespuesta e ((a,b):xs)
  | e == a     = b
  | otherwise  = getRespuesta e xs
  
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

-- | transitaMoore: dado el inicial el automata procesa la cadena
transitaMoore :: Moore -> [Alfabeto] -> [Estado]
transitaMoore m s = inicial (automataMoore  m) :transitaAux (inicial (automataMoore m))
  (transiciones (automataMoore m)) s

-- | traduceMoore: Obtiene la traudcción del automáta
traduceMoore :: Moore -> [Alfabeto] -> Alfabeto
traduceMoore m s = getRespuesta (last (transitaMoore m s)) (fRespuestas m)

esFinal :: Moore -> Estado -> Bool
esFinal m e = elem e $ finales (automataMoore m)

-- | aceptaMoore :: determina si una cadena es aceptada en el automáta o no.
aceptaMoore :: Moore -> [Alfabeto] -> Bool
aceptaMoore m s = esFinal m (last list) && length list == length s +1
  where list = transitaMoore m s
