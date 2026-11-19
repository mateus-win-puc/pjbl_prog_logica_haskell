module Leitura where

import Data.Maybe (listToMaybe)
import Data.Either (partitionEithers)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Tipos
import Classificacao


data Erro
  = CamposInvalidos Int String
  | NumeroInvalido Int String
  | PlacarNegativo Int
  | MesmoTime Int
  | TimeRepetidoNaRodada Int Time
  deriving (Show, Eq)

separar :: Char -> String -> [String]
separar c s = case break (== c) s of
  (a, [])     -> [a]
  (a, _:resto) -> a : separar c resto


-- rod = rodada, mand = mandante, gm = gols mandante, gv = gols visitante, vis = visitante
lerPartida :: Int -> String -> Either Erro Partida
lerPartida n linha = do
  let campos = map trim (separar ';' linha)
  (rodStr, mand, gmStr, gvStr, vis) <- case campos of
    [a,b,c,d,e] -> Right (a,b,c,d,e)
    _           -> Left (CamposInvalidos n linha)
  rod <- maybe (Left (NumeroInvalido n rodStr)) Right (readMaybe rodStr)
  gm  <- maybe (Left (NumeroInvalido n gmStr))  Right (readMaybe gmStr)
  gv  <- maybe (Left (NumeroInvalido n gvStr))  Right (readMaybe gvStr)
  if gm < 0 || gv < 0
    then Left (PlacarNegativo n)
    else if mand == vis
      then Left (MesmoTime n)
      else Right (Partida rod mand gm gv vis)
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- verifica se algum time joga duas vezes na mesma rodada
verificarRodadas :: [Partida] -> Either Erro ()
verificarRodadas partida = case duplicado of
  Just (r, t) -> Left (TimeRepetidoNaRodada r t)
  Nothing     -> Right ()
  where
    pares = concatMap (\p -> [(rodada p, mandante p), (rodada p, visitante p)]) partida
    duplicado = listToMaybe
      [ (r, t) | (r, t) <- pares, length (filter (== (r,t)) pares) > 1]

lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato texto = do
  let linhas = filter (not . ignorar) (lines texto)
      numeradas = zip [1..] linhas
  partidas <- traverse (uncurry lerPartida) numeradas
  verificarRodadas partidas
  return partidas
  where
    ignorar l = null (dropWhile isSpace l) || take 1 (dropWhile isSpace l) == "#"

todosOsErros :: String -> [Erro]
todosOsErros texto =
  let linhas = filter (not . ignorar) (lines texto)
      numeradas = zip [1..] linhas
      resultados = map (uncurry lerPartida) numeradas
      (erros, _) = partitionEithers resultados
  in erros
  where
    ignorar l = null (dropWhile isSpace l) || take 1 (dropWhile isSpace l) == "#"

tabelaFormatada :: [Linha] -> String
tabelaFormatada ls = cabecalho ++ concatMap formatar (zip [1..] ls)
  where
    cabecalho = "Pos | Time           | Pts | J | V | E | D | Saldo\n"
             ++ replicate 55 '-'
             ++ "\n"
    formatar (i, l) =
      let pos = pad 3 (show i)
          t   = pad 14 (timeLinha l)
          p   = pad 3 (show (pontosLinha l))
          j   = pad 1 (show (jogos l))
          v   = pad 1 (show (vitorias l))
          e   = pad 1 (show (empates l))
          d   = pad 1 (show (derrotas l))
          s   = show (saldo l)
      in pos ++ " | " ++ t ++ " | " ++ p ++ " | " ++ j ++ " | "
         ++ v ++ " | " ++ e ++ " | " ++ d ++ " | " ++ s ++ "\n"
    pad n s = s ++ replicate (n - length s) ' '