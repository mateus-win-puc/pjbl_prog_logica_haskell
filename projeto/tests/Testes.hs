module Testes where

import Tipos
import Classificacao
import Leitura
import Dados

testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
  | obtido == esperado = putStrLn ("OK      " ++ nome)
  | otherwise          = putStrLn ("FALHOU  " ++ nome ++ ": " ++ show obtido)


-- criando partida para os testes
partidaExemplo :: Partida
partidaExemplo = Partida 1 "coritiba" 2 1 "atletico"

testesCamada1 :: IO ()
testesCamada1 = do
  putStrLn "\n--- Camada 1 ---"

  -- resultadoPara: mandante venceu
  testar "resultadoPara mandante vencedor"
    (resultadoPara "coritiba" partidaExemplo) (Just Vitoria)

  -- resultadoPara: visitante perdeu
  testar "resultadoPara visitante perdedor"
    (resultadoPara "atletico" partidaExemplo) (Just Derrota)

  -- resultadoPara: time que nao jogou
  testar "resultadoPara time que nao jogou"
    (resultadoPara "parana" partidaExemplo) Nothing

  -- resultadoPara: empate
  let partidaEmpate = Partida 1 "A" 0 0 "B"
  testar "resultadoPara empate mandante"
    (resultadoPara "A" partidaEmpate) (Just Empate)
  testar "resultadoPara empate visitante"
    (resultadoPara "B" partidaEmpate) (Just Empate)

  -- participou: mandante, visitante e nao jogou
  testar "participou mandante"
    (participou "coritiba" partidaExemplo) True
  testar "participou visitante"
    (participou "atletico" partidaExemplo) True
  testar "participou time que nao jogou"
    (participou "parana" partidaExemplo) False

  -- times: sem repeticao e em ordem alfabetica
  case lerCampeonato dadosCampeonato of
    Left erro      -> putStrLn ("FALHOU  lerCampeonato no teste de times: " ++ show erro)
    Right partidas -> do
      testar "times do campeonato (ordem alfabetica, sem repeticao)"
        (times partidas)
        ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]
      testar "times nao tem duplicatas"
        (length (times partidas) == length (nubOrd (times partidas))) True

  -- edge case: lista vazia
  testar "times de lista vazia"
    (times []) []

-- camada 2


-- criando tabela de classificacao para exemplos
classificacaoEsperada :: [Linha]
classificacaoEsperada =
  [ Linha "A" 12 12 0 0 21 0,
  Linha "F" 12  5 5 2 13 8,
  Linha "H" 12  4 5 3 10 14,
  Linha "B" 12  4 4 4 12 10,
  Linha "C" 12  3 7 2 14 11,
  Linha "D" 12  3 6 3  9 9,
  Linha "G" 12  3 6 3  9 11,
  Linha "I" 12  2 6 4  8 10,
  Linha "E" 12  1 7 4  9 13,
  Linha "J" 12  0 0 12 0 19
  ]

testesCamada2 :: IO ()
testesCamada2 = do
  putStrLn "\n--- Camada 2---"

  case lerCampeonato dadosCampeonato of
    Left erro      -> putStrLn ("FALHOU  lerCampeonato no teste de classificacao: " ++ show erro)
    Right partidas -> do

      -- cassificacao completa
      testar "classificacao completa do campeonato"
        (classificacao partidas) classificacaoEsperada

      -- classificacao apos a rodada 3 diferente da final
      let parcial = classificacao (ateRodada 3 partidas)
      testar "classificacao da rodada 3 diferente da final"
        (parcial /= classificacao partidas) True

      --testando se na rodada 3 A eh lider
      case parcial of
        (lider:_) -> testar "lider da rodada 3 e A"
                       (timeLinha lider) "A"
        []        -> putStrLn "FALHOU  lider da rodada 3: tabela vazia"

      -- maiorGoleada e mediaGols com lista vazia -> Nothing
      testar "maiorGoleada [] = Nothing"
        (maiorGoleada []) Nothing
      testar "mediaGols [] = Nothing"
        (mediaGols []) Nothing

      -- testar se eh maior goleada
      testar "maiorGoleada do campeonato"
        (maiorGoleada partidas)
        (Just (Partida 3 "A" 3 0 "H"))

      -- testar media de gols
      testar "mediaGols do campeonato"
        (mediaGols partidas) (Just 1.75)

  -- teste de desempate - dois times com mesmos pontos, decidido por saldo de gols
  let partidasDesempate =
        [ Partida 1 "A" 2 0 "C",
          Partida 1 "B" 1 0 "D"
        ]
      tabela = classificacao partidasDesempate
  testar "desempate por saldo: A antes de B"
    (map timeLinha (take 2 tabela)) ["A", "B"]

  -- Desempate por vitorias
  -- A vem antes de B por ter mais vitorias
  let partidasVitorias =
        [ Partida 1 "A" 3 1 "C",
         Partida 2 "A" 0 1 "D",
         Partida 1 "B" 1 1 "E",
         Partida 2 "B" 1 1 "F",
         Partida 3 "B" 1 1 "G"
        ]
      tabelaV = classificacao partidasVitorias
      nomesV = map timeLinha tabelaV
      posA = length (takeWhile (/= "A") nomesV)
      posB = length (takeWhile (/= "B") nomesV)
  testar "desempate por vitorias: A antes de B"
    (posA < posB) True

  -- edgecase - campeonato vazio
  testar "classificacao de campeonato vazio"
    (classificacao []) []

-- Camada 3

testesCamada3 :: IO ()
testesCamada3 = do
  putStrLn "\n--- Camada 3 ---"

  -- lerCampeonato sobre a entrada valida devolvendo right
  case lerCampeonato dadosCampeonato of
    Left erro      -> putStrLn ("FALHOU  lerCampeonato valido: " ++ show erro)
    Right partidas -> testar "lerCampeonato valido devolve Right"
                        (length partidas) 60

  -- testes para construtor de erro
  testar "erro CamposInvalidos (linha com campos a menos)"
    (lerPartida 1 "1;A;2;1") (Left (CamposInvalidos 1 "1;A;2;1"))

  testar "erro NumeroInvalido (placar 'dois')"
    (lerPartida 2 "1;A;dois;0;B") (Left (NumeroInvalido 2 "dois"))

  testar "erro NumeroInvalido (rodada nao numerica)"
    (lerPartida 3 "x;A;0;0;B") (Left (NumeroInvalido 3 "x"))

  testar "erro PlacarNegativo"
    (lerPartida 4 "1;A;-1;0;B") (Left (PlacarNegativo 4))

  testar "erro MesmoTime"
    (lerPartida 5 "1;A;0;0;A") (Left (MesmoTime 5))

  -- TimeRepetidoNaRodada so e detectado por lerCampeonato (verificarRodadas)
  -- A jogando duas vezes na mesma rodada
  let entradaTimeRepetido = unlines
        [ "1;A;1;0;B"
        , "1;A;2;0;C"
        ]
  testar "erro TimeRepetidoNaRodada"
    (lerCampeonato entradaTimeRepetido) (Left (TimeRepetidoNaRodada 1 "A"))

  -- todosOsErros - texto com pelo menos 3 linhas invalidas
  let entradaComVariosErros = unlines
        [ "1;A;2;1;B",
         "1;A;0;0;A" ,      -- mesmo time
         "2;C;dois;1;D"  ,  -- numero invalido
         "2;E;-1;1;F" ,     -- negativo
         "linha_ruim"      -- campo invalido
        ]
  testar "todosOsErros lista os 4 erros"
    (todosOsErros entradaComVariosErros)
    [ MesmoTime 2, 
    NumeroInvalido 3 "dois",
    PlacarNegativo 4,
    CamposInvalidos 5 "linha_ruim"
    ]

  -- edge cases - texto vazio e so comentarios
  testar "lerCampeonato de texto vazio"
    (lerCampeonato "") (Right [])
  testar "lerCampeonato de so comentarios"
    (lerCampeonato "# comentario\n# outro\n") (Right [])
  testar "todosOsErros de texto valido"
    (todosOsErros "1;A;2;1;B") []

  -- linhas em branco e comentarios sao ignorados
  let entradaComBranco = unlines
        [ "# cabecalho",
        "",
        "1;A;2;1;B",
        "   ",
        "2;C;0;0;D"
        ]

  testar "lerCampeonato ignora branco e comentarios"
    (lerCampeonato entradaComBranco)
    (Right [ Partida 1 "A" 2 1 "B"
           , Partida 2 "C" 0 0 "D" ])



-- remove duplicatas preservando a ordem
nubOrd :: Eq a => [a] -> [a]
nubOrd [] = []
nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)



main :: IO ()
main = do
  putStrLn "================"
  testesCamada1
  testesCamada2
  testesCamada3
  putStrLn "\n================"