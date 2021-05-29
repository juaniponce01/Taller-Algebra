
-- Funciones que nos servirán para mas adelante
--------------------------------------------------------------------------------------------------

-- La m pasará por todos los números desde (n-1) hasta 1, intentará probar que es falso, hasta entonces será verdadero
esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux n 1 = True -- Si logra llegar al 1 es porque ya evaluó contra todos los números y siguió siendo verdadero
esPrimoAux n m | n `mod` m /= 0 = esPrimoAux n (m - 1) 
               | otherwise = False

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n (n-1) 

esCompuesto :: Integer -> Bool
esCompuesto 1 = False -- El 1 no es ni primo ni compuesto
esCompuesto n = not (esPrimo n)

--------------------------------------------------------------------------------------------------

-- La 'n' es nuesto común divisor
sonCoprimosAux2 :: Integer -> Integer -> Integer -> Bool
sonCoprimosAux2 a b n | b `mod` n == 0 = False 
                      | otherwise = sonCoprimosAux a b (n-1) -- Busquemos otro común divisor

-- Buscamos nuestro común divisor y lo almacenamos en 'n'
sonCoprimosAux :: Integer -> Integer -> Integer -> Bool
sonCoprimosAux a b 1 = True
sonCoprimosAux a b n | a `mod` n /= 0 = sonCoprimosAux a b (n-1)
                     | otherwise = sonCoprimosAux2 a b n -- Probemos nuestro común divisor 'n' en 'b'

-- Si los dos son primos o son el mismo número, entonces acá lo termina de resolver
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | a == b = False
                | esPrimo a && esPrimo b = True
                | a > b && a `mod` b == 0 = False -- Evaluamos con el número mismo
                | b > a && b `mod` a == 0 = False -- Evaluamos con el número mismo
                | otherwise = sonCoprimosAux a b (a-1)

-- Fórmula general para cualquier 'a'
esApseudoprimo :: Integer -> Integer -> Bool
esApseudoprimo n a = esCompuesto n && (a^(n-1) - 1) `mod` n == 0 

-- Probamos nuestra fórmula general con a = 2
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esApseudoprimo n 2

-- La 'n' empezará probando desde 1 hasta 'm' inclusive
cant3Hasta :: Integer -> Integer -> Integer
cant3Hasta m n | m < n = 0 -- Verificamos que sea hasta 'm' inclusive
               | esApseudoprimo n 3 = 1 + cant3Hasta m (n+1) 
               | otherwise = 0 + cant3Hasta m (n+1) 

-- La 'm' definirá hasta qué número puedo contabilizar
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = cant3Hasta m 1

-- La 'n' será el contador y 'm' hará pasar la funcion por todos los números
kesimo2y3PseudoprimoAux :: Integer -> Integer -> Integer -> Integer
kesimo2y3PseudoprimoAux k n m | k == n = (m-1) -- Cuando el contador llegue hasta el numero que le pedimos, dejará de contar
                              | es2Pseudoprimo m && esApseudoprimo m 3 = kesimo2y3PseudoprimoAux k (n+1) (m+1)
                              | otherwise = kesimo2y3PseudoprimoAux k n (m+1)

-- La 'k' definirá hasta cuál número debe llegar el contador
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoAux k 0 1

-- La función intentará probar la falsedad, hasta entonces será verdadero
esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux n 1 = True -- Si logra llegar al 1 es porque ya evaluó contra todos los números y siguió siendo verdadero
esCarmichaelAux n a | not(sonCoprimos n a) = esCarmichaelAux n (a-1) -- Si no son coprimos entonces no evalúa y pasa al siguiente
                    | esApseudoprimo n a = esCarmichaelAux n (a-1)
                    | otherwise = False

esCarmichael :: Integer -> Bool
esCarmichael n | esPrimo n = False -- Filtro; sólo los números compuestos pasan
               | otherwise = esCarmichaelAux n (n-1)

















 