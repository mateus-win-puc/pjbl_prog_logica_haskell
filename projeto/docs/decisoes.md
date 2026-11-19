## 1 Partida como record, não tupla

Decisão: Partida é um record com campos nomeados e não uma tupla

Justificativa: Records evitam o erro trocar os gols do mandante com os do visitante

## 2 Resultado como tipo soma

Decisão: data Resultado = Vitoria | Empate | Derrota.

## 3 Linha como record

Decisão: Linha é um record com timeLinha, jogos, vitorias, empates, derrotas, golsPro, golsContra.

Justificativa: Mesma razão da Partida: nomes nos campos evitam confusão entre golsPro e golsContra

## 4 resultadoPara devolve Maybe Resultado

Decisão: resultadoPara :: Time -> Partida -> Maybe Resultado devolve Nothing quando o time nao jogou a partida.

Justificativa: Maybe torna a ausência de resultado parte do tipo e obriga quem chama a tratar isso

## 5 foldl em vez de foldr em linhaDoTime

Decisão: linhaDoTime ps t = foldl (acumular t) (linhaVazia t) ps.

Justificativa: foi utilizado o foldl ao invés do foldr pois a leitura é mais natural

## 6 linhaVazia como valor inicial da dobra

Decisão: linhaVazia t = Linha t 0 0 0 0 0 0 é uma função, ao invez de escrever os zeros diretamente no foldl.

Justificativa: Melhoria da legibilidade

## 7 acumular isola o cálculo de gols feitos/sofridos no where

Decisão: Em acumular, golsFeitos e golsSofridos são calculados no where, com if t == mandante p....

Justificativa: Isolar essa lógica evita a repeticao em golsPro e golsContra e deixa explícito o tipo do time (mandante ou visitante)

## 8 aproveitamento usa fromIntegral e trata jogos == 0

Decisão: aproveitamento converte com fromIntegral (casting) antes de dividir e devolve 0 quando jogos == 0

Justificativa: / não aceita Int; fromIntegral faz a conversão para Double. A guarda jogos == 0 evita divisão por zero e devolve o valor correto (0%) para um time que não jogou

## 9 maiorGoleada e mediaGols devolvem Maybe

Decisão: Ambas devolvem Nothing para lista vazia.

Justificativa: para funções como foldr não quebrarem em tempo de execução

## 10 Either Erro em vez de Maybe na leitura

Decisão: lerPartida e lerCampeonato devolvem Either Erro, não Maybe Partida

Justificativa: Foi utilizado either para permitir mensagens de erro descritivas

## 11 traverse em lerCampeonato

Decisão: lerCampeonato usa traverse

Justificativa: traverse aplica lerPartida a todas as linhas e coleta os resultados em Either Erro [Partida] parando no primeiro Left

## 12 verificarRodadas separada de lerPartida

Decisão: A checagem de time repetido na mesma rodada é feita por verificarRodadas, chamada por lerCampeonato

Justificativa: lerPartida valida uma linha isolada enquanto verificarRodadas valida o conjunto de partidas, separando as responsabilidades mantendo lerPartida simples

## LIMITAÇÕES

## 13 todosOsErros não detecta TimeRepetidoNaRodada

Limitação: todosOsErros chama apenas lerPartida em cada linha, não verificarRodadas
Por isso, um texto com um time jogando duas vezes na mesma rodada não produz TimeRepetidoNaRodada no relatório de todosOsErros
esse erro só é detectado por lerCampeonato.

Justificativa: todosOsErros foi feito para listar erros de linha. A checagem de rodada é uma validação de conjunto e fica em lerCampeonato

## 14 Numeração de linhas ignora brancos e comentários

Limitação: O número da linha usado nas mensagens de erro é a posição após filtrar linhas em branco e comentários, não a posição no arquivo original. Se o arquivo tem um comentário na linha 1 e uma partida inválida na linha 3, o erro vai dizer "linha 2", não "linha 3".

Justificativa: A numeração é feita com zip [1..] sobre as linhas já filtradas. Numerar antes do filtro exigiria rastrear os números originais, o que complicaria o código

## 15 aproveitamento pode passar de 100% com dados inconsistentes

Limitação: Se os dados tiverem mais vitórias do que jogos (devido a erro de input manual), aproveitamento pode devolver um valor maior que 100.

## 16 main lê os dados do código, não de arquivo

Limitação: O main usa dadosCampeonato fixo no código

## 17 separar é recursiva

Limitação: separar usa recursão explícita

Justificativa: Pelo o que entendemos, a recursão explícita é proibida só para a Camada 2

## 18 Edge cases

Edge cases cobertos pelo Campeonato de Teste

O campeonato em Dados.hs tem 10 times e 12 rodadas, cobrindo:

Empate em 0x0: D 0x0 G (rodada 1)

Vitória por 3+ gols: A 3x0 H (rodada 3) e H 0x3 A (rodada 12)

Time com 100% de aproveitamento: A (36 pts em 12 jogos)

Time com 0% de aproveitamento: J (0 pts em 12 jogos)

Vitórias de visitante: A vence J-A, I-A, H-A como visitante

Empates com gols: vários (1x1, 2x2)

Desempate por vitórias: B (16 pts, 4V) antes de C (16 pts, 3V), mesmo C tendo saldo melhor (+3 vs +2)

Desempate por saldo: D (15 pts, saldo 0) antes de G (15 pts, saldo -2), ambos com 3 vitórias

Desempate por gols pró: na tabela da rodada 3, E e I empatam em 1 pt, ambos com saldo -2; E tem mais gols pró (2 vs 1) e vem antes
