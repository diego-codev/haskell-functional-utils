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
-- ex.: ordenada [3,7,7,8,9] True

ordenada :: Ord t => [t] -> Bool
ordenada (c:c2:r)
    | c <= c2 = ordenada (c2:r)
    | otherwise = False
ordenada [c] = True

-- =======================================================================================================

-- Questão 24

-- picos: recebe uma lista de números e retorna os números que são maiores que seus vizinhos.
-- Considere que a lista é circular, ou seja, o início e o fim estão ligados.
-- ex.: picos [2,3,5,10,5,5,6,2,3] [10,6,3]

picos :: Ord t => [t] -> [t]
picos l@(c:r) = analizar (ultimo l : l ++ [c] )
    where
        analizar (c1:c2:c3:r)
            | c2 > c1 && c2 > c3 = c2 : analizar (c2:c3:r)
            | otherwise = analizar (c2:c3:r)
        analizar _ = []

        ultimo :: [t] -> t
        ultimo [c] = c
        ultimo (_:r) = ultimo r
        
 -- =======================================================================================================

-- Questão 27

-- todas_maiusculas: Recebe uma string qualquer e retorna outra string onde todas as letras são
-- maiúsculas. Pode ser útil saber os seguintes códigos de representação de caracteres: a=97,
-- z=122, A=65, Z=90, 0=48, 9=57, espaço=32.
-- ex.: todas_maiusculas "abc 123" = "ABC 123"  

todas_maiusculas :: String -> String
todas_maiusculas [] = []
todas_maiusculas (c:r) = converte c : todas_maiusculas r
    where
        converte 'a' = 'A'
        converte 'b' = 'B'
        converte 'c' = 'C'
        converte 'd' = 'D'
        converte 'e' = 'E'
        converte 'f' = 'F'
        converte 'g' = 'G'
        converte 'h' = 'H'
        converte 'i' = 'I'
        converte 'j' = 'J'
        converte 'k' = 'K'
        converte 'l' = 'L'
        converte 'm' = 'M'
        converte 'n' = 'N'
        converte 'o' = 'O'
        converte 'p' = 'P'
        converte 'q' = 'Q'
        converte 'r' = 'R'
        converte 's' = 'S'
        converte 't' = 'T'
        converte 'u' = 'U'
        converte 'v' = 'V'
        converte 'w' = 'W'
        converte 'x' = 'X'
        converte 'y' = 'Y'
        converte 'z' = 'Z'
        converte c   = c

-- =======================================================================================================

-- Questão 30

-- =======================================================================================================

-- Questão 33

-- =======================================================================================================

-- Questão 36

-- =======================================================================================================

-- Questão 39

-- =======================================================================================================
