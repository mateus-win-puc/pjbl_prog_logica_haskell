module Classificacao where


import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Tipos

pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- linha vazia como valor inicial da dobra
linhaVazia :: Time -> Linha
linhaVazia t = Linha t 0 0 0 0 0 0

-- Atualiza a linha de um time, se nao jogou, devolve a linha intacta, caso contrario soma os valores
acumular :: Time -> Linha -> Partida -> Linha
acumular t l p = case resultadoPara t p of
  Nothing -> l
  Just r  -> l
    { jogos = jogos l + 1,
    vitorias = vitorias l + (if r == Vitoria then 1 else 0),
    empates = empates l  + (if r == Empate  then 1 else 0),
    derrotas = derrotas l + (if r == Derrota then 1 else 0),
    golsPro = golsPro l + golsFeitos,
     golsContra = golsContra l + golsSofridos
    }
  where
    golsFeitos   = if t == mandante p then golsMandante p else golsVisitante p
    golsSofridos = if t == mandante p then golsVisitante p else golsMandante p


linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime p t = foldl (acumular t) (linhaVazia t) p


pontosLinha :: Linha -> Int
pontosLinha l = 3 * vitorias l + empates l

saldo :: Linha -> Int
saldo l = golsPro l - golsContra l


-- ''casting'' (termo do java) para evitar erros
aproveitamento :: Linha -> Double
aproveitamento l
  | jogos l == 0 = 0
  | otherwise = fromIntegral (pontosLinha l) / fromIntegral (3 * jogos l) * 100

-- desempate: pontos > vitórias > saldo > gols pró > nome
-- usamos down para inverter a ordem
compararLinha :: Linha -> Linha -> Ordering
compararLinha = comparing (Down . pontosLinha)
            <> comparing (Down . vitorias)
            <> comparing (Down . saldo)
            <> comparing (Down . golsPro)
            <> comparing timeLinha



classificacao :: [Partida] -> [Linha]
classificacao ps = sortBy compararLinha (map (linhaDoTime ps) (times ps))

ateRodada :: Int -> [Partida] -> [Partida]
ateRodada n = filter ((<= n) . rodada)

maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada [] = Nothing
maiorGoleada m = Just (foldr1 melhor m)
  where
    dif x = abs (golsMandante x - golsVisitante x)
    melhor a b = if dif a >= dif b then a else b

mediaGols :: [Partida] -> Maybe Double
mediaGols [] = Nothing
mediaGols m = Just (fromIntegral total / fromIntegral (length m))
  where
    total = sum (map (\p -> golsMandante p + golsVisitante p) m)