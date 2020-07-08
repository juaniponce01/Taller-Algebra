
module Clase_adicional
where

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

esPrimo :: Int -> Bool
esPrimo n = (n > 1) && not (tieneDivisorDesde n 2)

tieneDivisorDesde :: Int -> Int -> Bool
tieneDivisorDesde n k | k == n = False
                      | otherwise = (n `mod` k == 0) || tieneDivisorDesde n (k+1)

longitud :: Int -> Int
longitud n = longitudDesde n 1

longitudDesde :: Int -> Int -> Int
longitudDesde 1 _ = 0
longitudDesde n k = 1 + (longitudDesde (n `div` (p^a)) (k+1))
    where p = nEsimoPrimo k
          a = quePotenciaLoDivide n p

quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide n p | n `mod` p == 0 = 1 + quePotenciaLoDivide (n `div` p) p
                        | otherwise = 0

iesimo :: Int -> Int -> Int
iesimo n i = quePotenciaLoDivide n (nEsimoPrimo i)

headN :: Int -> Int
headN n = iesimo n 1
