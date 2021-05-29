module Clase05
where
  
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

--sumaDivisoresHasta :: Int -> Int -> Int
--sumaDivisoresHasta n k | k == n = n
--                       | mod n k == 0 = k + sumaDivisoresHasta n (k + 1)
--                       | otherwise = sumaDivisoresHasta n (k + 1) 
--
--sumaDivisores :: Int -> Int
--sumaDivisores n = sumaDivisoresHasta n 1

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n m| m == 1 = 1
                      | n `mod` m == 0 = m + sumaDivisoresHasta n (m-1)
                      | otherwise = sumaDivisoresHasta n (m-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

menorFactDesdeAux :: Int -> Int -> Int
menorFactDesdeAux m n | (fact n) >= m = fact n
                      | otherwise = menorFactDesdeAux m (n + 1)

menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeAux m 1

mayorFactHastaAux :: Int -> Int -> Int
mayorFactHastaAux m n | (fact n) == m = m
                      | (fact n) > m = (fact (n - 1))
                      | otherwise = mayorFactHastaAux m (n + 1)

mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaAux m 1

esFact :: Int -> Bool
esFact 0 = False
esFact n = mayorFactHasta n == n 

-- Ejercicio #9
fibonacci :: Int -> Int 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

mayorFibHastaAux :: Int -> Int -> Bool
mayorFibHastaAux n m | n == fibonacci m = True
                     | n > fibonacci m = mayorFibHastaAux n (m + 1)
                     | otherwise = False

esFibonacci :: Int -> Bool
esFibonacci n = mayorFibHastaAux n 1

-- Ejercicio #10
esPrimoAux :: Int -> Int -> Bool
esPrimoAux n m | n /= 2 && n `mod` m == 0 = False
               | n /= 3 && n `mod` 3 == 0 = False
               | otherwise = True

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2 

esSumaInicialDePrimosAux :: Int -> Int -> Int -> Bool
esSumaInicialDePrimosAux m n o | o > 100 = False
                               | esPrimo m = True
                               | esPrimo n && esPrimo o && n + o == m = True
                               | esPrimo n /= True = esSumaInicialDePrimosAux m (n+1) o 
                               | esPrimo o /= True = esSumaInicialDePrimosAux m n (o+1)
                               | otherwise =  esSumaInicialDePrimosAux m (n+1) (o+1)


esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos m = esSumaInicialDePrimosAux m 2 3

-- Ejercicio #11 y #12
tomaValorMaxAux :: Int -> Int -> Int -> Int
tomaValorMaxAux n m o| n <= m = o
                     | sumaDivisores n > sumaDivisores o = tomaValorMaxAux (n-1) m n
                     | otherwise = tomaValorMaxAux (n-1) m o

tomaValorMax :: Int -> Int -> Int 
tomaValorMax n m = tomaValorMaxAux n m 1 -- PREGUNTAR!!!!!

-- Ejercicio #13
todasSumasDePrimosN :: Int -> Int -> Int -> Bool
todasSumasDePrimosN 1 m o = False
todasSumasDePrimosN n m o| not(esPrimo m) = False
                         | not(esPrimo n) = todasSumasDePrimosN (n-1) m o
                         | otherwise = n + m == o || todasSumasDePrimosN (n-1) m o

todasSumasDePrimosM :: Int -> Int -> Int -> Bool
todasSumasDePrimosM n 1 o = False
todasSumasDePrimosM n m o| not(todasSumasDePrimosN n m o) = todasSumasDePrimosM n (m-1) o
                         | otherwise = True

esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n = todasSumasDePrimosM n n n

-- Ejercicio #14 resuelto con el #13

-- Ejercicio #15
primosGemAux :: Int -> Int -> Int
primosGemAux 1 b = 0
primosGemAux a b | esPrimo a && esPrimo (a-2) = 1 + primosGemAux (a-1) (b-1)
                 | otherwise = primosGemAux (a-1) b 

primosGem :: Int -> Int
primosGem n = primosGemAux n n

-- Ejercicio #16
proxPrimosGemDesde :: Int -> Int -> (Int, Int)
proxPrimosGemDesde a b | esPrimo a && esPrimo (b+2) = (a, b+2)
                       | not(esPrimo a) = proxPrimosGemDesde (a+1) (b+1)
                       | otherwise = proxPrimosGemDesde (a+1) (b+1)

proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem n = proxPrimosGemDesde (n+1) (n+1)

-- Ejercicio #17
largoSecuencia :: Int -> Int
largoSecuencia 1 = 0
largoSecuencia n | (-1)^n == 1 = 1 + largoSecuencia (div n 2)
                 | (-1)^n == -1 = 1 + largoSecuencia (3 * n + 1)

mayorSecuencia :: Int -> Int -> Int
mayorSecuencia 0 m = m
mayorSecuencia n m | largoSecuencia n > largoSecuencia m = mayorSecuencia (n-1) n
                   | otherwise = mayorSecuencia (n-1) m

laSecuencia :: Int -> Int
laSecuencia n = mayorSecuencia n 1


