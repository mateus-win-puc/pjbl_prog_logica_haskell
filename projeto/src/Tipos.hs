module Tipos where

import Data.List (sort, sortBy, nub)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Either (partitionEithers)
import Data.Ord (comparing, Down(..))
import Text.Read (readMaybe)
import Data.Char (isSpace)


type Time = String

data Resultado = Vitoria | Empate | Derrota
  deriving (Show, Eq)

data Partida = Partida
  { rodada :: Int, mandante :: Time, golsMandante  :: Int, golsVisitante :: Int, visitante     :: Time
  } deriving (Show, Eq)

data Linha = Linha
  { timeLinha    :: Time, jogos :: Int, vitorias :: Int, empates :: Int, derrotas  :: Int, golsPro :: Int, golsContra :: Int
  } deriving (Show, Eq)

participou :: Time -> Partida -> Bool
participou t p = t == mandante p || t == visitante p

resultadoPara :: Time -> Partida -> Maybe Resultado
resultadoPara t p
  | t == mandante p =
      Just (if golsMandante p > golsVisitante p then Vitoria
            else if golsMandante p == golsVisitante p then Empate
            else Derrota)
  | t == visitante p =
      Just (if golsVisitante p > golsMandante p then Vitoria
            else if golsVisitante p == golsMandante p then Empate
            else Derrota)
  | otherwise = Nothing

times :: [Partida] -> [Time]
times t = sort (nub (concat (map (\p -> [mandante p, visitante p]) t)))