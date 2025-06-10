-- ======================================

-- Diego ALves de OLiveira - 202410370

-- Matheus Gomes Monteiro - 202410369

-- Grupo 0

-- ======================================

-- Questão 3

-- unica_ocorrencia: recebe um elemento e uma lista e verifica se existe uma única ocorrência
-- do elemento na lista.

-- ex.:
-- unica_ocorrencia 2 [1,2,3,2] ⇒ False
-- unica_ocorrencia 2 [3,1] ⇒ False
-- unica_ocorrencia 2 [2] ⇒ True 

unica_ocorrencia :: (Eq t) => t -> [t] -> Bool
unica_ocorrencia e (c:r)
    | e == c    = not (pertence e r) -- Caso 'e' pertença a lista, verifica se ela NÃO pertence novamente.
    | otherwise = unica_ocorrencia e r
unica_ocorrencia _ [] = False

pertence :: Eq t => t -> [t] -> Bool -- Função auxiliar. Exposta pois usamos novamente na questão 15.
pertence e (c:r)
    | e == c    = True
    | otherwise = pertence e r
pertence _ [] = False

-- =======================================================================================================

-- Questão 6

-- remove: recebe um elemento e uma lista e retorna a lista sem a primeira ocorrência do
-- elemento (se o elemento não estiver na lista, não há resposta possível)

remove :: (Eq t) => t -> [t] -> [t]
remove e (c:r)
    | e == c    = r -- remove a primeira ocorrência
    | otherwise = c : remove e r -- mantém todos os elementos anteriores ao 'e'
-- Caso lista vazia não implementado, assume-se que o elemento está na lista (pré-condição)

-- =======================================================================================================

-- Questão 9

-- gera_sequencia: recebe um número inteiro n positivo e retorna a lista [1,-1,2,-2,3,-3, ... ,n,-n]

gera_sequencia :: Int -> [Int]
gera_sequencia n
    | n == 1    = [1, -1]
    | otherwise = gera_sequencia (n - 1) ++ [n, -n]

-- =======================================================================================================

-- Questão 12

-- reverso: recebe uma lista e retorna outra, que contém os mesmos elementos da primeira, em
-- ordem contrária.

reverso :: [t] -> [t]
reverso [] = []
reverso (c:r) = reverso r ++ [c]

-- =======================================================================================================

-- Questão 15

-- somatorio: recebe uma lista de números e retorna a soma deles.

-- ex.:
-- somatorio [] ⇒ 0
-- somatorio [2,3] ⇒ 5

somatorio :: Num t => [t] -> t
somatorio [] = 0
somatorio (c:r) = c + somatorio r

-- =======================================================================================================

-- Questão 18

-- interseccao: recebe duas listas sem elementos repetidos e retorna uma lista com os
-- elementos que são comuns às duas.

-- ex.:
-- interseccao [3,6,5,7] [9,7,5,1,3] [3,5,7]

interseccao :: Eq t => [t] -> [t] -> [t]  
interseccao (c1:r1) l2
    | pertence c1 l2 = c1 : interseccao r1 l2
    | otherwise = interseccao r1 l2
interseccao [] _ = []

-- =======================================================================================================

-- Questão 21

-- ordenada: recebe uma lista e verifica se seus itens estão ordenados (ordem crescente).

-- ex.:
-- ordenada [3,7,7,8,9] True

ordenada :: Ord t => [t] -> Bool
ordenada (c:c2:r)
    | c <= c2 = ordenada (c2:r)
    | otherwise = False
ordenada [c] = True

-- =======================================================================================================

-- Questão 24

-- picos: recebe uma lista de números e retorna os números que são maiores que seus vizinhos.
-- Considere que a lista é circular, ou seja, o início e o fim estão ligados.

-- ex.:
-- picos [2,3,5,10,5,5,6,2,3] [10,6,3]

picos :: Ord t => [t] -> [t]
picos l@(c:r) = analisar (ultimo l : l ++ [c] )
    where
        analisar (c1:c2:c3:r)
            | c2 > c1 && c2 > c3 = c2 : analisar (c2:c3:r)
            | otherwise = analisar (c2:c3:r)
        analisar _ = []

ultimo :: [t] -> t
ultimo [c] = c
ultimo (c:r) = ultimo r
        
 -- =======================================================================================================

-- Questão 27

-- todas_maiusculas: Recebe uma string qualquer e retorna outra string onde todas as letras são
-- maiúsculas. Pode ser útil saber os seguintes códigos de representação de caracteres: a=97,
-- z=122, A=65, Z=90, 0=48, 9=57, espaço=32.

-- ex.:
-- todas_maiusculas "abc 123" = "ABC 123"  

todas_maiusculas :: String -> String
todas_maiusculas = map paraMaiuscula
  where
    paraMaiuscula c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

-- =======================================================================================================

-- Questão 30

-- 30. variancia: recebe uma lista de números e retorna a variância (populacional) deles.

-- ex.:
-- variancia [6,2,9,0,8,3,0,2] 10.6875

--variancia :: Num t => [t] -> t
--variancia (c:r) = 

variancia :: Fractional t => [t] -> t
variancia l =
    let m = media l                               -- calcula a média da lista
        diferencas = calcularDiferencas l m       -- lista das diferenças ao quadrado em relação à média
        n = fromIntegral (tamanhoLista l)         -- tamanho da lista convertido para Fractional
    in somatorio diferencas / n                   -- soma das diferenças dividida pelo tamanho
        where
            -- Função auxiliar para calcular a lista das diferenças ao quadrado em relação à média
            calcularDiferencas :: Fractional t => [t] -> t -> [t]
            calcularDiferencas [] _ = []
            calcularDiferencas (c:r) m = (c - m) ^ 2 : calcularDiferencas r m

media :: Fractional t => [t] -> t
media l = somatorio l / fromIntegral (tamanhoLista l)

tamanhoLista :: [t] -> Int
tamanhoLista [] = 0
tamanhoLista (c:r) = 1 + tamanhoLista r

-- =======================================================================================================

-- Questão 33

-- separa: separa os elementos de uma lista de números nas posições com zero.
-- ex.: separa [3,4,7,-1,0,4,7,3,0,0,9,8] [[3,4,7,-1],[4,7,3],[],[9,8]] 

separa :: (Integral t) => [t] -> [[t]]
separa l = separaAux l []
  where
    separaAux [] acc = [acc] -- fim da lista, retorna sublista acumulada (na ordem certa)
    separaAux (c1:r) acc
      | c1 == 0  = acc : separaAux r [] -- achou zero: finaliza sublista atual e começa nova vazia
      | otherwise = separaAux r (acc ++ [c1]) -- acumula elemento no final da sublista


-- =======================================================================================================

-- Questão 36

-- soma_digitos: Recebe um número natural e retorna a soma dos seus dígitos.
-- Exemplo:
-- soma_digitos 328464584658 = 63

soma_digitos :: (Integral t) => t -> t
soma_digitos n
    | n < 10    = n                            -- Caso base: número de um dígito
    | otherwise = (n `mod` 10) + soma_digitos (n `quot` 10) -- Soma o último dígito e chama recursivamente pro resto
 
-- =======================================================================================================

-- Questão 39

-- Dizemos que um quadrado perfeito é um número cuja raiz quadrada é um número inteiro.
-- Sabemos o que a raiz quadrada é um cálculo lento quando comparado à operações como
-- adição ou multiplicação. Implemente uma função que verifica se um número é um quadrado
-- perfeito sem usar uma função que calcula raiz quadrada. 

quadradoPerfeito :: (Integral t) => t -> Bool
quadradoPerfeito n = verifica n 1
  where
    verifica resto impar
        | resto == 0 = True   -- encontrou soma exata => quadrado perfeito
        | resto < 0  = False  -- passou do valor => não é quadrado perfeito
        | otherwise  = verifica (resto - impar) (impar + 2) -- subtrai o próximo ímpar

-- =======================================================================================================
