module Clase08
where

type Set a = [a]

pertenece :: Int -> Set Int -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

minimoCAux :: [Int] -> [[Int]] -> [Int]
minimoCAux n [] = n
minimoCAux n (x:xs) | n < x = minimoCAux n xs 
                    | otherwise = minimoCAux x xs

minimoC :: [[Int]] -> [Int]
minimoC (x:xs) = minimoCAux x xs

quitarC :: [Int] -> [[Int]] -> [[Int]]
quitarC n [] = []
quitarC n (x:xs) | iguales n x = xs
                 | otherwise = x : quitarC n xs

ordenarC :: Set (Set Int) -> Set (Set Int)
ordenarC [] = []
ordenarC (x:xs) = minimoCAux x xs : ordenarC (quitarC (minimoC (x:xs)) (x:xs))

quitar :: Int -> [Int] -> [Int]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

ordenar :: Set Int -> Set Int
ordenar [] = []
ordenar c = minimum c : ordenar (quitar (minimum c) c)

crearListaN :: Int -> Set Int
crearListaN 0 = []
crearListaN n = ordenar (n : crearListaN (n-1))

crearListaB :: Int -> Int -> String
crearListaB _ 0 = ""
crearListaB n m = 'b' : crearListaB n (m-1)

crearListaA :: Int -> Int -> String
crearListaA 0 m = 'b' : crearListaB 0 (m-1)
crearListaA n m = 'a' : crearListaA (n-1) m

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n : c

union :: Eq a => Set a -> Set a -> Set a
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante x [] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)  
 
agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = (agregarElementoAdelante x c) `union` (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n 1 = n : l
insertarEn l n i = (head l) : insertarEn (tail l) n (i-1)

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = insertarEnCadaPos xs c (length xs + 1) `union` (insertarEnCadaPosDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = ordenarC (variaciones (crearListaN k) n)

tiene1 :: Set [Int] -> Set [Int]
tiene1 [] = []
tiene1 (cs:css) | elem 1 cs = [cs] `union` tiene1 css
                | otherwise = tiene1 css

bolitascon1 :: Int -> Int -> Set [Int]
bolitascon1 n k = ordenarC (tiene1 (variaciones (crearListaN k) n))

listasK :: Int -> Set [Int]
listasK n = bolitasEnCajas n n

--caractAB :: Int -> Int -> Set Int
--caractAB n m = permutaciones (crearListaA n m)

--subjconjuntos :: Set Int -> Int -> Set (Set Int)
--subjconjuntos 





