module Clase07
where
    
type Set a = [a]

pertenece :: Int -> Set Int -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

agregar :: Int -> Set Int -> Set Int
agregar x xs | x `pertenece` xs = xs
             | otherwise = x:xs

quitar :: Int -> [Int] -> [Int]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

quitarC :: [Int] -> [[Int]] -> [[Int]]
quitarC n [] = []
quitarC n (x:xs) | iguales n x = xs
                 | otherwise = x : quitarC n xs

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n [] = []
quitarTodas n (x:xs) | n == x = quitar n xs
                     | otherwise = x : quitar n xs

minimoAux :: Int -> [Int] -> Int
minimoAux n [] = n
minimoAux n (x:xs) | n < x = minimoAux n xs 
                   | otherwise = minimoAux x xs

minimo :: [Int] -> Int
minimo (x:xs) = minimoAux x xs

minimoCAux :: [Int] -> [[Int]] -> [Int]
minimoCAux n [] = n
minimoCAux n (x:xs) | n < x = minimoCAux n xs 
                    | otherwise = minimoCAux x xs

minimoC :: [[Int]] -> [Int]
minimoC (x:xs) = minimoCAux x xs

ordenar :: Set Int -> Set Int
ordenar [] = []
ordenar (x:xs) = minimoAux x xs : ordenar (quitar (minimo (x:xs)) (x:xs))

ordenarC :: Set (Set Int) -> Set (Set Int)
ordenarC [] = []
ordenarC (x:xs) = minimoCAux x xs : ordenarC (quitarC (minimoC (x:xs)) (x:xs))

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

union :: Set Int -> Set Int -> Set Int
union [] c2 = c2
union (x:xs) c2 | pertenece x c2 = union xs c2
                | otherwise = union xs (x:c2)

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c2 = c2
unionC (x:xs) c2 | perteneceC x c2 = unionC xs c2
                 | otherwise = unionC xs (x:c2)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] c2 = []
interseccion (x:xs) c2 | pertenece x c2 = x : interseccion xs c2
                       | otherwise = interseccion xs c2

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] c2 = []
diferencia (x:xs) c2 | pertenece x c2 = diferencia xs c2
                     | otherwise = x : diferencia xs c2

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = ordenar (diferencia (union c1 c2) (interseccion c1 c2))

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | xs `perteneceC` xss = xss
                | otherwise = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs) 

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = ordenarC (unionC (partes xs) (agregarATodos x (partes xs)))

partesNaux :: Int -> Set Int
partesNaux 0 = []
partesNaux n = ordenar (n : partesNaux (n-1))

partesN :: Int -> Set (Set Int)
partesN n = partes (partesNaux n)







