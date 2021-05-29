bact :: Int -> Int
bact n | n <= 1 = 1
       | otherwise = bact (n-1) + bact (n-2)
       
fibo = bact       

ida :: Int -> Int
ida 0 = 0
ida n = 1 + ida (n-1)

idb :: Int -> Int
idb 0 = 0
idb n = (-1) + idb (n+1)
esPar1 :: Int -> Bool
esPar1 0 = True
esPar1 1 = False
esPar1 n = esPar1 (n-2)

esPar2 :: Int -> Bool
esPar2 0 = True
esPar2 1 = False
esPar2 n = esPar2 (n+2) && esPar2 (n-2)

unoAdelanteTresAtras :: Int -> Int
unoAdelanteTresAtras 0 = 0
unoAdelanteTresAtras 1 = 1
unoAdelanteTresAtras n | esPar n = 1 + unoAdelanteTresAtras (n+1)
                       | otherwise = 1 + unoAdelanteTresAtras (n-3) 

collatz 1 = 1
collatz n | n `mod` 2 == 0 = collatz (n `div` 2)
          | otherwise = collatz (3*n+1)
                       
esPar :: Int -> Bool
esPar n = parAux (abs n)
    where parAux n = n == 0 || not (parAux (n-1))                       
          

puntajeValido :: Int -> Bool
--opcion 1
puntajeValido 0 = True
puntajeValido n = (n >= 7 && puntajeValido(n-7)) || (n >= 11 && puntajeValido(n-11))
--opcion 2: cortocircuito
puntajeValido' :: Int -> Bool
puntajeValido' n = (n==0) || (n>=7 && puntajeValido'(n-7)) || (n>=11 && puntajeValido'(n-11))          

cantDigitos :: Int -> Int
cantDigitos 0 = 0
cantDigitos n = 1 + cantDigitos (n `div` 10)

esBinario :: Int -> Bool
esBinario 0 = True
esBinario n = n `mod` 10 <= 1 && esBinario (n `div` 10)

esParR :: Int -> Bool                   
esParR 0 = True                      
esParR n = esImparR (n-1)

esImparR :: Int -> Bool       
esImparR 0 = False
esImparR n = esParR (n-1)

bact' :: Int -> Int                      
bact' 0 = 1                              
bact' n = bact' (n-1) + repr (n-1)        

repr :: Int -> Int
repr 0 = 0
repr n = bact' (n-1)

dosParams :: Int -> Int -> Int
dosParams 0 _ = 0
dosParams _ 0 = 0
dosParams m n | m `mod` 2 == 0 = n + dosParams (m-2) (n+1)
              | otherwise = n + dosParams (m+2) (n-1)

parametroMcontrola :: Int -> Int -> Int
parametroMcontrola 0 _ = 0
parametroMcontrola m n = n + (parametroMcontrola (m `div` 2) (2*n))

---------------------------------------------------------------------

tribonacci :: Int -> Int
tribonacci n | n <= 2 = n
    | otherwise = tribonacci(n - 1) + tribonacci(n - 2) + tribonacci(n - 3)

mult3 :: Int -> Bool
mult3 3 = True
mult3 n = n > 0 && mult3 (n - 3)

diabolico :: Int -> Bool
diabolico 6 = True
diabolico n = n > 0 && diabolico (n `div` 10) && (n `mod` 10) == 6

digiguales :: Int -> Bool
digiguales n | n < 10 = True
             | otherwise = (n `mod` 10) == (n `div` 10) `mod` 10 && digiguales(n `div` 10)

resta :: Int -> Int -> Int
resta 0 _ = 0
resta n 0 = n
resta n m = resta (pred n) (pred m)  

menor :: Int -> Int -> Bool
menor 0 0 = False
menor 0 m = True
menor n 0 = False
menor n m = menor (resta n m ) (resta m n)

mayor :: Int -> Int -> Bool
mayor 0 0 = False
mayor n 0 = True
mayor 0 m = False
mayor n m = mayor (resta n m ) (resta m n)

iguales :: Int -> Int -> Bool
iguales 0 0 = True
iguales n 0 = False
iguales 0 m = False
iguales n m = iguales (resta n m) (resta n m)

esPotencia :: Int -> Int -> Bool
esPotencia n m = n == 1 || n >= m && esPotencia (div n m) m && n `mod` m == 0



