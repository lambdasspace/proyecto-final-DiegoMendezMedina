module Automata where

type Estado = String
type Alfabeto = Char
type Transicion = (Estado, Alfabeto) -> Estado
type Respuesta  =  Estado -> Alfabeto

type Automata = Moore 

data Moore = Moore{
  estados :: [Estado],
  alfabetoEntrada :: [Alfabeto],
  alfabetoSalida  :: [Alfabeto],
  funcionTransicion :: [Transicion],
  funcionRespuesta :: Respuesta,
  inicial :: Estado,
  finales :: [Estado]
  }


