tienenDivComun :: Integer -> Integer -> Integer -> Bool --Determina si a o b tienen un divisor común diferente de 1.
tienenDivComun a b counter| counter > a || counter > b = False
                          | a `mod` counter == 0 && b `mod` counter == 0 && not(counter == 1) = True
                          | otherwise = tienenDivComun a b (counter + 1) --Recurre aumentando el contador.

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = not(tienenDivComun a b 1) 

esPrimoAux :: Integer -> Integer -> Bool --Determina primalidad recurriendo por todos los números menores o iguales que a, tratando de encontrar un divisor.
esPrimoAux 1 counter = False
esPrimoAux a counter|counter >= a = True 
                    | counter == 1 || not (a `mod` counter == 0) = esPrimoAux a (counter + 1)
                    | a `mod` counter == 0 = False
--Nota: para evitar saltarme algún caso borde sin querer, lo hago correr hasta counter == a; pero perfectamente podría ser hasta counter == sqrt(a) + 1 y sería más óptimo. 

esPrimo :: Integer -> Bool
esPrimo a = esPrimoAux a 1

esAPseudoprimo :: Integer -> Integer -> Bool --Función general de Pseudoprimalidad para evitar repetir código.
esAPseudoprimo n a = not(esPrimo n || n == 1) && (a^(n-1) - 1) `mod` n == 0

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esAPseudoprimo n 2

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos 1 = 0
cantidad3Pseudoprimos m| esAPseudoprimo m 3 = 1 + cantidad3Pseudoprimos (m-1)
                       | otherwise = cantidad3Pseudoprimos (m-1)    

kesimo2y3PseudoprimoAux :: Integer -> Integer -> Integer -> Integer
kesimo2y3PseudoprimoAux m counter k| k == counter = (m-1)
                                | es2Pseudoprimo m && esAPseudoprimo m 3 = kesimo2y3PseudoprimoAux (m+1) (counter + 1) k
                                | otherwise = kesimo2y3PseudoprimoAux (m+1) counter k

kesimo2y3Pseudoprimo :: Integer -> Integer    
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoAux 1 0 k

esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux n counter| counter == (n - 1) = esAPseudoprimo n counter 
                         | sonCoprimos n counter = esAPseudoprimo n counter && esCarmichaelAux n (counter + 1)
                         | otherwise = esCarmichaelAux n (counter + 1)

esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelAux n 1