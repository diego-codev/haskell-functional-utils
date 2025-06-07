-- ======================================

-- Diego ALves de OLiveira - 202410370

-- Matheus Gomes Monteiro - 202410369

-- Grupo 0

-- ======================================

-- Questão 3

-- unica_ocorrencia: recebe um elemento e uma lista e verifica se existe uma única ocorrência
-- do elemento na lista.
-- ex.: unica_ocorrencia 2 [1,2,3,2] ⇒ False
-- unica_ocorrencia 2 [3,1] ⇒ False
-- unica_ocorrencia 2 [2] ⇒ True 

unica_ocorrencia :: (Eq t) => t -> [t] -> Bool
unica_ocorrencia e (c:r)
    | e == c    = not (ocorre_novamente e r) -- verifica se o elemento NÃO tem mais de uma ocorrência
    | otherwise = unica_ocorrencia e r
    where ocorre_novamente e (c:r) -- Função auxiliar escondida para ser usada só pela unica_ocorrencia
        | e == c    = True
        | otherwise = ocorre_novamente e r
    ocorre_novamente _ [] = False
unica_ocorrencia _ [] = False
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
unica_ocorrencia2 :: (Eq t) => t -> [t] -> Bool
unica_ocorrencia2 e (c:r)
    | e == c    = not (ocorre_novamente e r) -- verifica se o elemento NÃO tem mais de uma ocorrência
    | otherwise = unica_ocorrencia2 e r
unica_ocorrencia2 _ [] = False
-- Questão 15

-- Questão 18

-- Questão 21

-- Questão 24

-- Questão 27

-- Questão 30

-- Questão 33

-- Questão 36

-- Questão 39