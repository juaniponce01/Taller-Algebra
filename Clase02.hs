suma :: (Float , Float) -> (Float , Float) -> (Float , Float)
suma (vx, vy) (wx, wy) = (vx + wx, vy + wy)

resta :: (Float , Float) -> (Float , Float) -> (Float , Float)
resta (vx, vy) (wx, wy) = (vx - wx, vy - wy)
 
-- |normaVectorial2 x y es la norma de (x,y)
normaVectorial2 :: Float -> Float -> Float 
normaVectorial2 x y = sqrt (x^2 + y^2)

-- |normaVectorial1 (x,y) es la norma de (x,y)
normaVectorial1 :: (Float , Float) -> Float 
normaVectorial1 (x,y) = sqrt (x^2 + y^2)

norma1Suma :: (Float , Float) -> (Float , Float) -> Float
norma1Suma v1 v2 = normaVectorial1 (suma v1 v2)

norma2Suma :: (Float , Float) -> (Float , Float) -> Float
norma2Suma v1 v2 = normaVectorial2 (fst s) (snd s) 
    where s = suma v1 v2

----------------------------------------------------------------
-- Decide si dos numeros se encuentran en el mismo conjunto (-inf, 3];(3, 7];(7, inf)
estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | x <= 3 = y <= 3
                      | x <= 7 = y <= 7
                      | x > 7 = y > 7

-- Devuelve el producto inerno de dos vectores
prodInt :: (Float , Float) -> (Float , Float) -> Float
prodInt (a, b) (c, d) = a * c + b * d

-- Dos vectores probando si el primero es menor que el segundo
todoMenor :: (Float , Float) -> (Float, Float) -> (Bool, Bool)
todoMenor v w = ((fst v) < (fst w), (snd v) < (snd w))

-- Calucula la distancia entre dos puntos en R^2
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float 
distanciaPuntos v1 v2 = normaVectorial1 (resta v1 v2)

-- Suma de una terna de elementos
sumaTerna :: Int -> Int -> Int -> Int
sumaTerna x y z = x + y + z

-- Devuelve la posicion del primer numero par
posicPrimerPar :: (Int,Int,Int) -> Int
posicPrimerPar (x,y,z) | (-1)^x > 0 = 1
                       | (-1)^y > 0 = 2
                       | (-1)^z > 0 = 3
                       | otherwise = 4

-- Crea un punto en R^2 
crearPar :: (Num a, Eq a, Num b, Eq b) => a -> b -> (a, b)
crearPar a b = (a, b)

-- Invierte los dos puntos que le asignas
invertir :: (Num a, Eq a, Num b, Eq b) => a -> b -> (b, a)
invertir a b = (b, a)

