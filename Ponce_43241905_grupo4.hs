-- Segundo Trabajo Práctico del Taller de Algebra 

import Data.Char (ord, chr)

---------------- Funciones de la Clase Adicional

esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && not (tieneDivisorDesde n 2)

tieneDivisorDesde :: Integer -> Integer -> Bool
tieneDivisorDesde n k | k == n = False
                      | otherwise = (n `mod` k == 0) || tieneDivisorDesde n (k+1)

---------------- Funciones de la Clase 9

mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (abs a, signum a, 0)
emcd a b = (d, tau, sigma - (a `div` b) * tau)
    where (d, sigma, tau) = emcd b (a `mod` b) 

---------------- Funciones de la Clase 10

ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

solucionEcConPropAdic :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m 

solucionEc :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

----------------------------------- TP ---------------------------------------------

-- Nos genera una clave pública (n,d) y una clave privada (n,e)
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | esPrimo p && esPrimo q && n > 127 = ((n,d), (n,e)) 
                  | otherwise = undefined -- si 'p' o 'q' no son primos o su multiplicacion es ≤ 127 entonces no se podrá seguir con el procedimiento
                  where n = p * q
                        m = (p - 1) * (q - 1) 
                        e = expDeDecifrado 2 m -- 'e' debe ser un número coprimo con 'm' entre 2 y ('m' - 2), pruebo con el más chico primero y me devuelve el mismo o uno más grande, dependiendo de si cumple o no con la condición
                        d = fst (solucionEc (e, 1, m)) -- 'd' se calcula resolviendo la ecuacion de congruencia 'd' * 'e' ≡ 1 (mod 'm')

-- Nos dará el exponente de decifrado: le damos el 2 y nos devuelve el número más chico que cumpla 'e' coprimo con 'm'
expDeDecifrado :: Integer -> Integer -> Integer
expDeDecifrado e m | e `mcd` m == 1 = e -- para que e sea comprimo con m, el máximo común divisor entre ambos debe ser 1
                   | otherwise = expDeDecifrado (e + 1) m -- seguirá probando con números más grandes hasta cumplir la condición

-- Encripta un mesaje en una lista de números dando la clave publica
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar (n, d) "" = [] -- una vez que ya no quedaron más caracteres, la lista encriptada de numeros se cierra
encriptar (n, d) (x:xs) = mod (a^d) n : encriptar (n, d) xs -- reemplazo cada número 'a' por el resto de la división de 'a'^'d' por 'n' y aplico la recursión para repetirlo con cada caracter
    where a = fromIntegral (ord x) -- la variable 'a' es el numero de 0 a 127 que le corresponde a cada caracter; luego lo convierto en Integer

-- Desencripta un menaje dando la clave privada y una lista de números
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar (n, e) [] = "" -- una vez que ya no quedaron más números, la lista de caracteres se cierra formando nuestro mensaje
desencriptar (n, e) (x:xs) = chr b : desencriptar (n, e) xs -- reemplazo cada número 'a' (en este caso asociada a la variable 'b') por su respectivo caracter y aplico la recursión para repetirlo con cada número
    where b = fromInteger (mod (x^e) n) -- la variable 'b' es el resto de dividir el numero encriptado a la 'e' por 'n', este resultado nos va a dar 'a', el verdadero numero de la letra que le corresponde; lo convierto en Int para que luego pueda usarlo con la funcion 'chr'

---------------------------- Ejercicio Adicional -------------------------------------

-- Pregunta: cual es tu pizza favorita?
-- Respuesta: [97672,18800,91658,91658,58255,77756,74457,58255,99961,58255,11695,28700,23881,220,58255]

------------------ Funciones que tuve que utilizar para desencriptar el mensaje:

-- Devuelve uno de los primos que forma a la variable 'n'
quePrimoLoDivide :: Integer -> Integer -> Integer
quePrimoLoDivide n k | n < (nEsimoPrimo k) = undefined -- si nuestro primo supera a 'n' significa que la 'n' que pusimos no es producto de dos primos
                     | mod n (nEsimoPrimo k) == 0 = nEsimoPrimo k -- una vez encontrado un primo que divida a 'n' ya tenemos nuestro 'p'
                     | otherwise = quePrimoLoDivide n (k+1) -- nos fijamos con el siguiente primo

-- Desencripta un mensaje dando una clave pública y una lista de números
desencriptarConClavePublica :: (Integer, Integer) -> [Integer] -> String
desencriptarConClavePublica (n, d) lista = desencriptar (n, e) lista -- una vez conseguida nuestra clave privada, podemos desencriptar el mensaje
    where e = fst (solucionEc (d, 1, m)) -- para encontrar 'e' busco el resto de la siguiente ecuacion de congruencia
          m = (p - 1) * (q - 1) 
          p = quePrimoLoDivide n 2 -- para encontrar 'p' busco uno de los dos primos que divide a 'n'
          q = div n p -- para encontrar 'q' alcanza con dividir el 'n' con el 'p' hallado

-- Funciones de la Clase Adicional que necesité para resolver este ejercicio
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)






