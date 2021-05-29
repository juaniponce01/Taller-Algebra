module Clase06
where
	
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (x + n) : sumarN n xs

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = (x + x) : sumarN x xs

ultimo :: [Int] -> Int
ultimo (x:xs) | longitud (x:xs) == 1 = x
              | otherwise = ultimo xs

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo (x:xs) = ultimo xs + x : sumarN (ultimo xs) xs

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise = pares xs

quitar :: Int -> [Int] -> [Int]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n [] = []
quitarTodas n (x:xs) | n == x = quitar n xs
                     | otherwise = x : quitar n xs

repetidos :: [Int] -> [Int]
repetidos [] = []
repetidos (x:xs) | pertenece x xs = x : repetidos xs
                 | otherwise = repetidos xs

cantRepe :: [Int] -> Int
cantRepe l = longitud (repetidos l)

hayRepetidos :: [Int] -> Bool
hayRepetidos l = cantRepe l >= 1

elimRepAlFinalAux :: [Int] -> [Int] -> [Int]

elimRepAlFinal :: [Int] -> [Int]
elimRepAlFinal l1 l2 = elimRepAlFinalAux l1 l2

elimRepAlInicioAux :: [Int] -> [Int] -> [Int]
elimRepAlInicioAux [] l = l
elimRepAlInicioAux (x:xs) l = elimRepAlInicioAux xs (quitar x l) 

elimRepAlInicio :: [Int] -> [Int]
elimRepAlInicio l = elimRepAlInicioAux (repetidos l) l

maximoAux :: Int -> [Int] -> Int
maximoAux n [] = n
maximoAux n (x:xs) | n < x = maximoAux x xs
                   | otherwise = maximoAux n xs

maximo :: [Int] -> Int
maximo (x:xs) = maximoAux x xs

minimoAux :: Int -> [Int] -> Int
minimoAux n [] = n
minimoAux n (x:xs) | n < x = minimoAux n xs 
                   | otherwise = minimoAux x xs

minimo :: [Int] -> Int
minimo (x:xs) = minimoAux x xs

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = minimoAux x xs : ordenar (quitar (minimo (x:xs)) (x:xs))

cuerpo :: [Int] -> [Int]
cuerpo [] = []
cuerpo (x:xs) | longitud (x:xs) == 1 = quitar x (x:xs)
              | otherwise = x : cuerpo xs

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = ultimo (x:xs) : reverso (cuerpo (x:xs))

concatenar :: [Int] -> [Int] -> [Int]
concatenar [] [] = []
concatenar [] (y:ys) = y : concatenar [] ys
concatenar (x:xs) (y:ys) = x : concatenar xs (y:ys)
























