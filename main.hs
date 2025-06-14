-- ======================================
-- Diego Alves de Oliveira - 202410370
-- Matheus Gomes Monteiro - 202410369
-- Grupo 0
-- ======================================

-- Questão 3
-- Verifica se um elemento ocorre exatamente uma vez na lista.

unica_ocorrencia :: (Eq t) => t -> [t] -> Bool
unica_ocorrencia e (c:r)
    | e == c    = not (pertence e r)
    | otherwise = unica_ocorrencia e r
unica_ocorrencia _ [] = False

-- Função auxiliar para verificar se um elemento pertence à lista.
pertence :: Eq t => t -> [t] -> Bool
pertence e (c:r)
    | e == c    = True
    | otherwise = pertence e r
pertence _ [] = False

-- =======================================================================================================

-- Questão 6
-- Remove a primeira ocorrência de um elemento em uma lista, se existir.

remove :: (Eq t) => t -> [t] -> [t]
remove e (c:r)
    | e == c    = r
    | otherwise = c : remove e r
remove _ [] = []

-- =======================================================================================================

-- Questão 9
-- Gera uma lista contendo os números de 1 a n e seus respectivos negativos.

gera_sequencia :: Int -> [Int]
gera_sequencia n
    | n == 1    = [1, -1]
    | otherwise = gera_sequencia (n - 1) ++ [n, -n]

-- =======================================================================================================

-- Questão 12
-- Retorna a lista na ordem inversa.

reverso :: [t] -> [t]
reverso []    = []
reverso (c:r) = reverso r ++ [c]

-- =======================================================================================================

-- Questão 15
-- Soma todos os elementos de uma lista.

somatorio :: Num t => [t] -> t
somatorio []    = 0
somatorio (c:r) = c + somatorio r

-- =======================================================================================================

-- Questão 18
-- Retorna a interseção entre duas listas, sem repetições.

interseccao :: Eq t => [t] -> [t] -> [t]
interseccao (c1:r1) l2
    | pertence c1 l2 = c1 : interseccao r1 l2
    | otherwise      = interseccao r1 l2
interseccao [] _ = []

-- =======================================================================================================

-- Questão 21
-- Verifica se a lista está em ordem crescente.

ordenada :: Ord t => [t] -> Bool
ordenada (c:c2:r)
    | c <= c2   = ordenada (c2:r)
    | otherwise = False
ordenada [_] = True

-- =======================================================================================================

-- Questão 24
-- Retorna os picos da lista (elementos maiores que os vizinhos).

picos :: Ord t => [t] -> [t]
picos (c:r) = extrairPicos (ultimo (c:r) : (c:r) ++ [c]) -- adiciona o último elemento no início e o primeiro no fim
    where
        -- Função auxiliar escondida que percorre trios consecutivos procurando picos
        extrairPicos (c1:c2:c3:r)
            | c2 > c1 && c2 > c3 = c2 : extrairPicos (c2:c3:r)
            | otherwise          = extrairPicos (c2:c3:r)       
        extrairPicos _ = []

-- Função auxiliar que retorna o ultimo elemento da lista.
ultimo :: [t] -> t
ultimo [c]   = c         
ultimo (c:r) = ultimo r   
-- =======================================================================================================

-- Questão 27
-- Converte todas as letras minúsculas de uma string em maiúsculas (sem usar Data.Char).

todas_maiusculas :: String -> String
todas_maiusculas = map paraMaiuscula
  where
    -- Função auxiliar escondida que converte caractere minúsculo para maiúsculo, se necessário.
    paraMaiuscula c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

-- =======================================================================================================

-- Questão 30
-- Calcula a variância de uma lista de números reais.

variancia :: RealFrac t => [t] -> t -- Usamos RealFrac ao invés de Fractional, seguindo sua recomendação
variancia l =
    let m = media l                           
        diferencas = calcularDiferencas l m
    in somatorio diferencas / fromIntegral (tamanhoLista l) 
        where
            -- Função auxiliar escondida que devolve lista das diferenças ao quadrado
            calcularDiferencas :: Num t => [t] -> t -> [t]
            calcularDiferencas [] _    = [] 
            calcularDiferencas (c:r) m = (c - m) ^ 2 : calcularDiferencas r m

-- Função auxiliar que calcula a média aritmética de uma lista.
media :: RealFrac t => [t] -> t
media l = somatorio l / fromIntegral (tamanhoLista l)

-- Função auxiliar que conta o número de elementos da lista.
tamanhoLista :: [t] -> Int
tamanhoLista []    = 0                
tamanhoLista (c:r) = 1 + tamanhoLista r

-- =======================================================================================================

-- Questão 33
-- Separa uma lista de inteiros em sublistas, quebrando nos zeros.

separa :: Integral t => [t] -> [[t]]
separa l = separaAux l []
    where
        -- Função auxiliar escondida que acumula sublistas e quebra no zero
        separaAux [] acc = [acc] -- fim da lista: retorna a última sublista acumulada
        separaAux (c:r) acc
            | c == 0    = acc : separaAux r []     -- se o elemento é zero, finaliza a sublista atual e começa nova
            | otherwise = separaAux r (acc ++ [c]) 

-- =======================================================================================================

-- Questão 36
-- Soma os dígitos de um número inteiro positivo.

soma_digitos :: Integral t => t -> t
soma_digitos n
    | n < 10    = n
    | otherwise = mod n 10 + soma_digitos (quot n 10) -- separa o último dígito e soma com os demais recursivamente

-- =======================================================================================================

-- Questão 39
-- Verifica se um número é um quadrado perfeito usando soma de ímpares.

quadradoPerfeito :: Integral t => t -> Bool
quadradoPerfeito n = verifica n 1
    where
        -- Função auxiliar escondida que subtrai ímpares sucessivos até 0 ou negativo
        verifica resto impar
            | resto == 0 = True
            | resto < 0  = False
            | otherwise  = verifica (resto - impar) (impar + 2)

-- =======================================================================================================
