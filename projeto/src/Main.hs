module Main where

import Tipos
import Classificacao
import Leitura
import Dados

main :: IO ()
main = do
  putStrLn "=== TABELA FINAL ==="
  case lerCampeonato dadosCampeonato of
    Left erro        -> print erro
    Right partidas   -> putStr (tabelaFormatada (classificacao partidas))

  putStrLn "\n=== TABELA APOS A RODADA 3 ==="
  case lerCampeonato dadosCampeonato of
    Left erro        -> print erro
    Right partidas   -> putStr (tabelaFormatada (classificacao (ateRodada 3 partidas)))

  putStrLn "\n=== ESTATISTICAS ==="
  case lerCampeonato dadosCampeonato of
    Left erro        -> print erro
    Right partidas   -> do
      putStr "Maior goleada: " >> print (maiorGoleada partidas)
      putStr "Media de gols: " >> print (mediaGols partidas)

  putStrLn "\n=== ENTRADA COM ERROS ==="
  let erros = unlines
        [ "1;A;2;1;B",
        "1;A;0;0;A",
        "2;C;dois;1;D",
        "2;E;-1;1;F",
        "3;G;1;0;H",
        "3;G;2;0;I",
        "linha_ruim"
        ]
  print (todosOsErros erros)