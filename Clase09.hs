module Clase09
where
	
digitos :: Integer -> Integer -> [Integer]
digitos n b | n <= 0 = []
            | otherwise = mod n b : digitos (div n b) b

numero :: [Integer] -> Integer -> Integer
numero (x:xs) b | null xs = x 
                | otherwise = b * (numero xs b) + x

mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

mcm :: Int -> Int -> Int
mcm a b = (a `div` d) * (b `div` d) * d
    where d = mcd a b 

emcd :: Int -> Int -> (Int, Int, Int)
emcd a 0 = (abs a, signum a, 0)
emcd a b = (d, tau, sigma - (a `div` b) * tau)
    where (d, sigma, tau) = emcd b (a `mod` b) 