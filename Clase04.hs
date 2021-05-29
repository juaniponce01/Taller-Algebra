g1 :: Int -> Int -> Int
g1 i 1 = 1
g1 i n = i^n + g1 i (n - 1)

g2 :: Int -> Int -> Int 
g2 i n = g1 i n + g1 i (n - 1)

g3Aux :: Int -> Int -> Int
g3Aux 1 n = 1
g3Aux i n = i^n + (g3Aux (i-1) n)

g3 :: Int -> Int
g3 1 = 1
g3 n = (g3Aux n n) + (g3 (n-1))

g4 :: Int -> Int
g4 1 = 0
g4 n| n `mod` 2 == 0 = 2^n + (g4 (n-1))
    | otherwise = (g4 (n-1))

digitosIguales :: Int -> Bool
digitosIguales n| n < 10 = True
                | otherwise = n `mod` 10 == (n `div` 10) `mod` 10 && digitosIguales(n `div` 10)

sumaIguales :: Int -> Int
sumaIguales 1 = 1
sumaIguales n| (digitosIguales n) = n + sumaIguales(n-1)
             | otherwise = sumaIguales(n-1)