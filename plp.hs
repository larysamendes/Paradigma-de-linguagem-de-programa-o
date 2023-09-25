--BUBBLESORT:
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if x > y then y : bubbleSort (x:xs) else x : bubbleSort (y:xs)


-- INSERTION SORT
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : insert x ys

--MERGESORT:
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))

firstHalf xs = take (div (length xs) 2) xs
secondHalf xs = drop (div (length xs) 2) xs

--QUICKSORT:
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]

-- [0,3,6,9,12,15]

list1:: [Int]
list1 = [x | x <- [0,3..15]]

-- Os múltiplos de 2 e 3 entre 0 e 20.

list2:: [Int]
list2 = [x | x <- [0..20], mod x 2 == 0, mod x 3 ==0]
--[[1],[2],[3],[4],[5]]

list3:: [[Int]]
list3 = [[x] | x <- [1..5]]

-- [[1], [1,1], [1,1,1], [1,1,1,1], [1,1,1,1,1]]

list4:: [[Int]]
list4 = [[1 | _ <- [1..n]] | n <- [1..5]]

--  [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]

list5:: [(Int, Int)]

list5 = [(x ,y ) | x <- [1..3], y <- [3,2..1]]

{-
Escreva uma função insere que receba um elemento e uma lista (tipagem genérica) e
insere o elemento se somente se ele ainda não pertence à lista. Em seguida faça os
seguintes testes:
> insere 2 [1,2,3]
> insere 7 [x+4|x<-[10,8..1]]
Como modificar a função para que possamos inserir ordenadamente elementos em listas
previamente ordenadas? Defina esta função.

-}


insert::  Int -> [Int] -> [Int]
insert a [] = [a]
insert a (s:sx)
    | a == s = s: sx
    | a < s = a : s : sx
    | otherwise = s : insert a sx

insert1:: Int -> [Int] -> [Int]
insert1 a [] = []
insert1 a list = [x | x <- list, x < a ] ++ [a] ++ [y | y <- list, y >= a]
{-
 Defina uma função recursiva que dada uma lista de inteiros, retorna uma nova lista
contendo os elementos de valor superior a um número n qualquer.
> retornaSup 4 [3,2,5,6]
[5,6]

-}
retornaSup:: Int -> [Int] -> [Int]
retornaSup num list = [x | x <- list, x > num]

{-
Função 'parImpar', que recebe uma lista de inteiros e retorna uma tupla cujo primeiro elemento é
uma lista dos números pares da lista de entrada e o segundo é uma lista dos números ímpares da
lista de entrada.
Ex.:
*Main> parImpar [1,2,3,4,5]
([2,4],[1,3,5])

-}

pares:: [Int] -> [Int]
pares [] = []
pares (a:ax)
    | mod a 2 == 0 = a : pares ax
    | otherwise = pares ax

impares:: [Int] -> [Int]
impares [] = []
impares (a:ax)
    | mod a 2 /= 0 = a : impares ax
    | otherwise = impares ax

parImpar:: [Int] -> ([Int],[Int])
parImpar list = (pares list, impares list)
    
{-
Função 'estaOrdenado', que recebe uma lista e retorna um Bool que indica se a lista está ordenada
ou não.
Ex.:
*Main> estaOrdenado [1,2,3]
-}

estaOrdenado:: [Int] -> Bool
estaOrdenado [] = True
estaOrdenado (a:[]) = True
estaOrdenado (a: b)
    | a <= (head b) = estaOrdenado b
    | otherwise = False

-- Quick Sort

quickSort:: [Int] -> [Int]
quickSort [] = []
quickSort (a:b) = left ++ [a] ++ right
    where
        left = quickSort $ filter (< a) b
        right = quickSort $ filter (>= a) b


{-
Função 'sort', que receba uma lista e a ordene.
Ex.:
*Main> sort [1,3,5,2,9,8,7]
[1,2,3,5,7,8,9]
-}


merge:: [Int] -> [Int] -> [Int]
merge [] [] = []
merge a [] = a
merge [] b = b
merge (a:ax) (b:bx)
    | a < b = a : merge ax (b:bx)
    | otherwise = b : merge bx (a:ax)


quickSort1:: [Int] ->[Int]
quickSort1 [] = []
quickSort1 (x:xs) = left ++ [x] ++ right
    where
        left = quickSort [a | a <- xs, a <= x]
        right = quickSort[a | a <- xs , a > x]

{-
8) Função 'getWord', que recebe uma String e retorna a primeira palavra dessa String – sem contar
pontuação.
Ex.:
*Main> getWord “Olá, Mundo!”
“Olá”
-}


getWord :: String -> String
getWord [] = []  -- Caso base: string vazia, retorna uma string vazia
getWord (x:xs)
    | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = x : getWord xs  -- Se x for uma letra, adiciona à palavra
    | otherwise = []  -- Se não for uma letra, encerra a palavra

{-
 Um crivo de Eratóstenes em Haskell – uma função que receba um número inteiro positivo n e
retorne uma lista com todos os números primos menores que n.
Ex.:
*Main> eratostenes 12
[2,3,5,7,11]
-}

ehPrimo:: Int -> Int -> Bool
ehPrimo num divisor
    | num == divisor = True
    | mod num divisor == 0 = False
    | otherwise = ehPrimo num (divisor +1)

eratostenes:: Int -> [Int]
eratostenes 0 = []
eratostenes num = [x | x <- [2..num], ehPrimo x 2]

{-
 Função 'split', que recebe uma função f do tipo (Int->Bool) e uma lista L de números inteiros e
retorna uma tupla onde o primeiro elemento é uma lista dos itens i de L tais que f i é verdadeiro e o
segundo elemento é uma lista dos itens i' de L tais que f i' é falso.
Ex.:
*Main> split (>5) [1,2,3,4,5,6,7,8]
([6,7,8],[1,2,3,4,5])
-}

split:: (Int -> Bool) -> [Int] -> ([Int], [Int])
split f list = ([ x | x <-list, f x ], [y | y <- list, not (f y)])

{-
nro-elementos: recebe uma lista qualquer e retorna o número de elementos na
lista.

-}

total:: [t] ->Int
total [] = 0
total (s:sx) = 1 + total sx

{-
maior: recebe uma lista de números e retorna o maior . 
-}

maior:: [Int] -> Int
maior [x] = x
maior (s:xs) = max s (maior xs)

{-
conta-ocorrencias: recebe um elemento e uma lista qualquer e retorna o número
de ocorrências do elemento na lista.
-}

conta_ocor:: Int -> [Int] -> Int
conta_ocor e list = length [x | x <- list, x == e]

conta_ocor2:: Int -> [Int] -> Int
conta_ocor2 e [] = 0
conta_ocor2 e (s: sx)
    | e == s = 1 + conta_ocor2 e sx
    | otherwise = conta_ocor2 e sx

{-
 unica-ocorrencia: recebe um elemento e uma lista e verifica se existe uma
única ocorrência do elemento na lista .
ex.:
unica-ocorrencia 2 [1,2,3,2] = False
unica-ocorrencia 2 [3,1] = False
unica-ocorrencia 2 [2] = True 
-}

unica_ocor:: Int -> [Int] -> Bool
unica_ocor elemento list
    | conta_ocor elemento list == 1 = True
    | otherwise = False


{-
maiores-que: recebe um número e uma lista de números e retorna uma lista com
os números que são maiores do que o valor informado.
ex.:
maiores-que 10 [4 6 30 3 15 3 10 7] ==> [30 15]
-}

maiores1:: Int -> [Int] -> [Int]
maiores1 num list = filter (\x -> x >= num) list

maiores_que:: Int -> [Int] -> [Int]
maiores_que num list = [x | x <- list, x > num]

list_a:: [String] -> [String]
list_a list = filter (\x -> head x == 'a') list

{-
 concatena: recebe duas listas quaisquer e retorna uma terceira lista com os
elementos da primeira no início e os elementos da segunda no fim.
ex.:
concatena [] [] ==> []
concatena [1 2] [3 4] ==> [1 2 3 4]
-}

concatena:: [t] -> [t] -> [t]
concatena list1 list2 = list1 ++ list2

{-
duplica: recebe uma lista e retorna uma nova lista contendo a duplicação dos
elementos da lista original.
ex: duplica [1, 2, 3] ==> [1,1,2,2,3,3]
-}

duplicata1:: [Int] -> [Int]
duplicata1 [] = []
duplicata1 (s:sx) = s : s : duplicata1 sx 

{-
Remove duplicata
-}


{-
2)	Defina uma função recursive de potência na qual power x y eleva à y-ésima potência.
power :: Int -> Int -> Int
-}

{-
3)	Defina zip :: [a] -> [b] -> [(a, b)], 
onde zip pega duas listas e concatena, 
de forma que o primeiro par da lista resultante seja composta pelos dois primeiros elementos de cada lista de entrada, 
e assim por diante. Ex: zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. Se uma das listas for maior que a outra,
 pode parar assim que a primeira acabar. Ex: zip [1,2] "abc" = [(1, 'a'), (2, 'b')].
-}

zip1:: [a] -> [b] -> [(a, b)]
zip1 [] [] = []
zip1 [] _ = []
zip1 _ [] = []
zip1 (a:ax) (b:bx) = (a,b) : zip1 ax bx 
quadrados:: [Int] -> [Int]
quadrados y = [x * x | x <- y]


dobrar:: [Int] -> [Int]
dobrar y  = [x *2 | x <- y]

dobrar1:: [Int] -> [Int]
dobrar1 [] = []
dobrar1 (s:sx) = [2*s] ++ dobrar1 (sx)

quadrados_pares:: [Int]
quadrados_pares  =[x * x| x <- [1..20], mod x 2 == 0]


pares:: Int -> Int -> Int -> Int -> [(Int, Int)]
pares a b c d = [(x,y)| x <- [a..b] , y <- [c..d]]

rotate:: [Int] -> Int -> [Int]
rotate list num = drop num list ++ take num list

rotate1:: [Int] -> Int -> [Int]
rotate1 [] _ = []
rotate1 l 1 = l
rotate1 (s:xs) num = rotate (xs ++ [s]) (num-1)

rotate2::  [Int] -> Int -> [Int]
rotate2 l n = take(length l) (drop n (cycle l))

reverse3:: [t] -> [t]
reverse3 [] = []
reverse3 (s:sx) = reverse sx ++ [s]


rep:: Int -> t -> [t]
rep 0 ch = []
rep n ch = ch : rep(n-1) ch

map1:: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <-xs]

par :: Int -> Bool
par num
    | mod num 2 == 0 = True
    | otherwise = False

apply:: (a -> a) -> a -> a
apply f x = f (f x)

incrementa:: Int -> Int
incrementa x = x+1

{- Defina a função addEspacos que produz um
string com uma quantidade n de espaços.
-}

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos total = " " ++ addEspacos (total -1)

{-
Defina a função paraDireita utilizando a definição
de addEspacos para adicionar uma quantidade n
de espaços à esquerda de uma dada String
-}

paraDireita :: Int -> String -> String
paraDireita 0 a = a
paraDireita total a = " " ++ paraDireita (total -1) a

{-
Defina uma função que calcula o tamanho de
uma lista
-}

legth1:: [t] -> Int
legth1 [] = 0
legth1 (s:sx) = 1 + legth1 sx

{-
• Defina uma função que encontra o i-ésimo
elemento de uma lista
-}

elemento:: [t] -> Int -> t
elemento (s:sx) 1 = s
elemento (s:sx) num = elemento sx (num -1)

{-
Defina uma função que encontra o maior
elemento de uma lista de inteiros
-}

maior:: [Int] -> Int
maior [x] = x
maior (x:xs) = max x (maior xs)

{-
• Defina uma função que recebe uma string e
remove tudo, exceto as letras maiúsculas dela
-}

removeNonUppercase:: String -> String
removeNonUppercase "" = ""
removeNonUppercase (s:sx) = if not(upper s) then removeNonUppercase sx
    else [s] ++ removeNonUppercase sx

upper:: Char -> Bool
upper c = find c ['A'..'Z']

find:: Char -> [Char] -> Bool
find _ [] = False
find caracter (x:xs) = if x == caracter then True
    else find caracter xs

removeMais:: String -> String
removeMais palavra = [x | x <- palavra, elem x ['A'..'Z']]

{-
Defina uma função que recebe uma lista de palavras, e retorna a
quantidade de palavras que estão no plural.
• Considere que qualquer palavra terminada em 's' está no plural.
-}

plural:: [String] -> Int
plural [] = 0
plural (s: sx) = if (last s == 's') then 1 + plural sx
    else
        plural sx

plural1:: [String ] -> Int
plural1 palavra = length [x | x <- palavra, last x == 's']

{-
Defina uma função que recebe uma lista de inteiros e
retorna uma lista com as somas dos elementos que
ocupam posições simétricas (primeiro + último, segundo
+ penúltimo, terceiro + antepenúltimo, …).
-}

sumSim::[Int] -> [Int]
sumSim (x: []) = [x]
sumSim (x:y:[]) = [x + y]
sumSim (h:t) = [h + (last t)] ++ sumSim (take ((length t) -1)t)

{-
• Defina uma função que recebe uma lista de listas e
retorna a lista dos elementos unificados
• flatten [[1, 2], [3, 4], [5, 6]] è [1, 2, 3, 4, 5, 6]
-}

flatter:: [[Int]] -> [Int]
flatter [] = []
flatter (x: sx) = x ++ flatter sx

{-
Defina uma função que recebe uma lista e um inteiro e
retorna uma tupla de duas listas, os n primeiros
elementos e o resto
• split 2 [1, 2, 3, 4] è ([1, 2], [3, 4])
-}

split:: Int -> [a] -> ([a],[a])
split num sx = (take num sx, drop num sx)

premio:: Int ->Int
premio 0 = 0
premio point
    | point >=1 && point <= 10 = 100
    | point <= 20 = 200
    | point <= 30 = 300
    | point <= 40 = 400
    | otherwise = 500

main:: IO()
main = do
    entrada <- readLn :: IO Int
    if (entrada > -1) then do
        main
        print entrada
        else
            print "Invertidos"

main1:: IO()
main1 = do
    str <- getLine
    total <- readLn:: IO Int
    repete str total

repete:: String -> Int -> IO()
repete palavra total =
    if (total > 1) then do 
        putStrLn palavra
        repete palavra (total -1)
        else do
            putStrLn palavra

ehprimo:: Int -> Bool
ehprimo 1 = False
ehprimo n = proxDiv n 2 == n

proxDiv:: Int -> Int -> Int
proxDiv n i
    |divisor n i = i
    |otherwise = proxDiv n (i+1)

divisor:: Int -> Int -> Bool
divisor n i
    | mod n i == 0 = True
    | otherwise = False


binario:: Int -> String
binario 0 = ""
binario num
    | mod num 2 == 1 = binario (div num 2) ++ "1"
    | otherwise = binario (div num 2 ) ++ "0"

ehpar:: Int -> Bool
ehpar x = mod x 2 == 0

pares_valores:: Int -> Int -> String
pares_valores inicio fim =
    if (inicio >= fim) then ""
    else if (inicio + 1) < fim && ehpar (inicio +1) then  show(inicio +1) ++ " " ++ (pares_valores (inicio+ 1) fim)
        else pares_valores (inicio +1) fim


